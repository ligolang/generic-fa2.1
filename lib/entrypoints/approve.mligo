#import "../data/errors.mligo" "Errors"
#import "../data/token.mligo" "Token"
#import "../data/approvals.mligo" "Approvals"
#import "../data/storage.mligo" "Storage"
#import "./events/approve.mligo" "Event"

type storage = Storage.t

type approve = [@layout:comb] {
      owner     : address;
      spender   : address;
      token_id  : Token.t;
      old_value : nat;
      new_value : nat;
   }   

type approvals = approve list
type t = approvals

let approve 
         (approve: approve) 
         (approvals: Approvals.t) 
         : Approvals.t =
   let { owner; spender; token_id; old_value; new_value } = approve in
   let amount = Approvals.get_amount approvals owner spender token_id in
   let () = assert_with_error (amount = old_value) Errors.unsafe_approval in
   let approvals = Approvals.set_amount approvals owner spender token_id new_value in
   approvals

let approve 
         (type a l) 
         (to_approve: approvals) 
         (storage: (a, l) storage) 
         : operation list * (a, l) storage =
   let approvals = List.fold_left (fun (approvals,a) -> approve a approvals) 
                                  (Storage.get_approvals storage) to_approve in
   let storage = Storage.set_approvals storage approvals in
   let event = Event.make_event to_approve in
   [ event ], storage
