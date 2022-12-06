#import "../data/errors.mligo" "Errors"
#import "../data/token.mligo" "Token"
#import "../data/ledger.mligo" "Ledger"
#import "../data/storage.mligo" "Storage"
#import "./events/transfer.mligo" "Event"

type storage = Storage.t

type imported_ticket = (Token.t * bytes option) ticket

type tickets_to_import_to = { 
      to_ : address; 
      tickets_to_import: imported_ticket list
   }

type import_ticket = tickets_to_import_to list

type t = import_ticket

type ledger = Ledger.t
type ledger_module = Ledger.ledger_module


[@inline]
let assert_ticketer_is_self_address (ticketer: address) : unit =
    assert_with_error (ticketer = (Tezos.get_self_address ())) Errors.invalid_ticket

let import_ticket_to 
            (type k v) 
            (to_:address) 
            (imported_ticket: imported_ticket)
            (transactions, ledger: Event.transaction list * (k,v) ledger_module)
            : Event.transaction list * (k,v) ledger_module =
    let (ticketer, ((token_id, _), amount)), _ = Tezos.read_ticket imported_ticket in
    let () = assert_ticketer_is_self_address ticketer in
    let ledger = Ledger.increase_token_amount_for_user ledger to_ token_id amount in
    let transaction = Event.make_transaction (Some to_) token_id amount in
    transaction :: transactions, ledger

let import_tickets_to
            (type k v)
            (tickets_to_import_to: tickets_to_import_to) 
            (transfers, ledger: Event.transfer list * (k,v) ledger_module) 
            : Event.transfer list * (k,v) ledger_module =
    let { to_; tickets_to_import } = tickets_to_import_to in
    let transactions, ledger = List.fold_left (fun (r,t) -> import_ticket_to to_ t r) 
                   ([], ledger) tickets_to_import in
    let transfer = Event.make_transfer None transactions in
    transfer :: transfers, ledger

let import_tickets 
            (type a k v) 
            (import_ticket : import_ticket) 
            (storage: (a,k,v) storage) 
            (ledger_module : (k,v) ledger_module) 
            : operation list * (a,k,v) storage =
    let transfers, ledger = List.fold_left (fun (r,t) -> import_tickets_to t r) 
                                     ([], ledger_module) import_ticket in
    let message = Event.make_event transfers in
    let event = Tezos.emit "%transfer_event" message in
    [ event ], Storage.set_ledger storage ledger.data
