#import "../data/errors.mligo" "Errors"
#import "../data/token.mligo" "Token"
#import "../data/ledger.mligo" "Ledger"
#import "../data/storage.mligo" "Storage"
#import "./events/transfer.mligo" "Event"

type storage = Storage.t

type exported_ticket = (nat * bytes option) ticket 

type ticket_to_export = [@layout:comb] {
      from_ : address;
      token_id : Token.t;
      amount : nat 
   }

type destination = [@layout:comb]
   | Single of exported_ticket contract
   | Multiple of exported_ticket list contract

type export_tickets_to = [@layout:comb] {
      destination : destination;
      tickets_to_export : ticket_to_export list
   }

type export_ticket = export_tickets_to list

type t = export_ticket

type ledger_module = Ledger.ledger_module

let create_ticket 
            (type l) 
            (ticket_to_export: ticket_to_export)
            (ledger: l ledger_module)
            : (Event.transfer * exported_ticket * l ledger_module ) = 
   let { from_; token_id; amount } = ticket_to_export in
   let ledger = Ledger.decrease_token_amount_for_user ledger from_ token_id amount in
   let ticket = Tezos.create_ticket (token_id, None) amount in
   let ticket = Option.unopt_with_error ticket Errors.cannot_create_ticket in
   (* Not efficient implementation: Should be grouped by from_ *)
   let transfer = Event.make_transfer (Some from_) [ Event.make_transaction None token_id amount ] in 
   transfer, ticket, ledger

let create_tickets 
            (type l) 
            (tickets_to_export: ticket_to_export list) 
            (transfers: Event.transfer list)
            (ledger: l ledger_module)
            : (Event.transfer list * exported_ticket list * l ledger_module) = 
   List.fold_left (fun ((ltr,lt,l),tk) -> let (tr,t,l) = create_ticket tk l in (tr::ltr,t::lt,l)) 
                  (transfers, [], ledger) tickets_to_export

let send_tickets_to 
            (destination: destination) 
            (tickets: exported_ticket list) 
            (ops: operation list) 
            : operation list =
   match destination with
   | Single contract -> 
      List.fold_left (fun (ops, ticket) -> Tezos.transaction ticket 0tez contract :: ops)
                     ops tickets
   | Multiple contract -> 
      Tezos.transaction tickets 0tez contract :: ops

let export_ticket_to 
            (type l) 
            (export_tickets_to: export_tickets_to) 
            (transfers, ops, ledger: Event.transfer list * operation list * l ledger_module)
            : (Event.transfer list * operation list * l ledger_module) = 
   let { destination; tickets_to_export } = export_tickets_to in
   let transfers, tickets, ledger = create_tickets tickets_to_export transfers ledger in
   let ops = send_tickets_to destination tickets ops in
   transfers, ops, ledger

let export_tickets
            (type a l) 
            (export_ticket: export_ticket) 
            (storage: (a,l) storage) 
            (ledger_module: l ledger_module) 
            : operation list * (a,l) storage =
   let transfers, ops, l = List.fold_left (fun (r,t) -> export_ticket_to t r) 
                               ([], [], ledger_module) export_ticket in
   let message = Event.make_event transfers in
   let event = Tezos.emit "%transfer_event" message in
   event :: ops, Storage.set_ledger storage l.data
