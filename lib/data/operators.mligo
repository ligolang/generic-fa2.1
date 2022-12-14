#import "errors.mligo" "Errors"
#import "token.mligo" "Token"

type owner    = address
type operator = address

type t = ((owner * Token.t), operator set) big_map

let assert_authorisation (operators : t) (from_ : address) (token_id : Token.t) : unit =
   let sender_ = (Tezos.get_sender ()) in
   if (sender_ = from_)
   then ()
   else
   let authorized =
      match Big_map.find_opt (from_, token_id) operators with
      | Some (v) -> v
      | None -> Set.empty
      in
      if Set.mem sender_ authorized
      then ()
      else failwith Errors.not_operator

let assert_update_permission (owner : owner) : unit =
   assert_with_error (owner = (Tezos.get_sender ())) Errors.only_sender_manage_operators

let add_operator (operators : t) (owner : owner) (operator : operator) (token_id : Token.t) : t =
   if owner = operator
   then operators
   else let () = assert_update_permission owner in
        let auth = match Big_map.find_opt (owner, token_id) operators with
            | Some (v) -> v
            | None -> Set.empty in
        let auth = Set.add operator auth in
        Big_map.update (owner,token_id) (Some auth) operators

let remove_operator (operators : t) (owner : owner) (operator : operator) (token_id : Token.t) : t =
   if owner = operator 
   then operators
   else let () = assert_update_permission owner in
        let auth =
            match Big_map.find_opt (owner, token_id) operators with
            | None -> None
            | Some (value) ->
               let ts = Set.remove operator value in
               if (Set.size ts = 0n)
               then None
               else Some (ts)
         in
         Big_map.update (owner, token_id) auth operators
