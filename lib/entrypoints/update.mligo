#import "../data/errors.mligo" "Errors"
#import "../data/operators.mligo" "Operators"
#import "../data/ledger.mligo" "Ledger"
#import "../data/storage.mligo" "Storage"
#import "./events/update_operators.mligo" "Event"

type storage = Storage.t

type operator = [@layout:comb] {
      owner    : address;
      operator : address;
      token_id : nat;
   }

type unit_update = 
   | Add_operator of operator 
   | Remove_operator of operator

type update_operators = unit_update list

type t = update_operators

let update_ops 
         (updates: update_operators) 
         (operators: Operators.t) 
         : Event.operator_update list * Operators.t =
   let update_operator ((updates,operators),update : (Event.operator_update list * Operators.t) * unit_update) = 
      match update with
      | Add_operator { owner ; operator ; token_id } -> 
            let operators = Operators.add_operator operators owner operator token_id in
            let update = Event.make_update owner operator token_id true in
            update :: updates, operators
      | Remove_operator { owner ; operator ; token_id} ->          
            let operators = Operators.remove_operator operators owner operator token_id in 
            let update = Event.make_update owner operator token_id false in
            update :: updates, operators
   in
   let updates, operators = List.fold_left update_operator ([], operators) updates in
   updates, operators
   
let update_ops 
         (type a l) 
         (updates: update_operators) 
         (storage: (a,l) storage) 
         : operation list * (a,l) storage =   
   match Storage.get_operators storage with
   | Some operators -> 
         let updates, operators = update_ops updates operators in 
         let storage = Storage.set_operators storage operators in
         let event = Event.make_event updates in
         [ event ], storage 
   | None -> failwith Errors.storage_has_no_operators
