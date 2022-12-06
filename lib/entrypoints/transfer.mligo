#import "../data/token.mligo" "Token"
#import "../data/amount.mligo" "Amount"
#import "../data/approvals.mligo" "Approvals"
#import "../data/operators.mligo" "Operators"
#import "../data/ledger.mligo" "Ledger"
#import "../data/storage.mligo" "Storage"

type storage = Storage.t

type atomic_trans = [@layout:comb] {
      to_      : address;
      amount   : Amount.t;
      token_id : Token.t;
   }

type transfer_from = {
      from_ : address;
      txs   : atomic_trans list
   }

type transfer = transfer_from list

type t = transfer
type ledger = Ledger.t
type ledger_module = Ledger.ledger_module

let authorize_transfer 
         (type a k v) 
         (storage: (a,k,v) storage) 
         (approvals:Approvals.t) 
         (from_: address) 
         (token_id: Token.t) 
         (amount: Amount.t) 
         : Approvals.t =
   match Storage.get_operators storage with
   | Some operators -> let () = Operators.assert_authorisation operators from_ token_id in approvals
   | None           -> Approvals.decrease_approved_amount approvals from_ (Tezos.get_sender ()) token_id amount

let atomic_trans 
         (type a k v) 
         (storage: (a,k,v) storage) 
         (from_:address) 
         ((ledger, approvals), transfer:((k,v) ledger_module * Approvals.t) * atomic_trans)
         : (k,v) ledger_module * Approvals.t =
   let { to_; token_id; amount = amount_ } = transfer in
   let ()        = Storage.assert_token_exist storage token_id in
   let approvals = authorize_transfer storage approvals from_ token_id amount_ in
   let ledger    = Ledger.decrease_token_amount_for_user ledger from_ token_id amount_ in
   let ledger    = Ledger.increase_token_amount_for_user ledger to_   token_id amount_ in
   ledger, approvals

let transfer_from 
      (type a k v) 
      (storage: (a,k,v) storage) 
      ((ledger, approvals), transfer : ((k,v) ledger_module * Approvals.t) * transfer_from)
      : (k,v) ledger_module * Approvals.t =
   let { from_; txs } = transfer in 
   List.fold_left (atomic_trans storage from_) (ledger, approvals) txs

let transfer 
      (type a k v) 
      (transfer: transfer) 
      (storage: (a,k,v) storage) 
      (ledger:(k,v) ledger_module)
      : operation list * (a,k,v) storage =
   let approvals = Storage.get_approvals storage in
   let ledger,approvals = List.fold_left (transfer_from storage) (ledger, approvals) transfer in
   let storage = Storage.set_approvals storage approvals in
   let storage = Storage.set_ledger storage ledger.data in
   [], storage
