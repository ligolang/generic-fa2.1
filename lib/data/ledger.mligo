#import "errors.mligo" "Errors"
#import "token.mligo" "Token"
#import "amount.mligo" "Amount"

type owner = address

type ('k,'v) t = ('k,'v) big_map

type ('k,'v) ledger_module = { 
    data: ('k,'v) t; 
    make_key  : address -> Token.t -> 'k;
    add_to_val: address -> nat -> 'v option -> 'v option;
    sub_to_val: address -> nat -> 'v option -> 'v option;
    balance_of: address -> 'v option -> nat;
}

let get_for_user 
      (type k v) 
      (ledger_module: (k,v) ledger_module) 
      (owner: owner) 
      (token_id: Token.t) 
      : v option =
  let key = ledger_module.make_key owner token_id in
  Big_map.find_opt key ledger_module.data

let set_for_user 
      (type k v) 
      (ledger_module: (k,v) ledger_module) 
      (owner: owner) 
      (token_id: Token.t) 
      (value:v option) 
      : (k,v) ledger_module =
  let key = ledger_module.make_key owner token_id in
  let data = Big_map.update key value ledger_module.data in
  { ledger_module with data }

let decrease_token_amount_for_user 
      (type k v) 
      (ledger_module: (k,v) ledger_module) 
      (from_: owner) 
      (token_id: Token.t) 
      (amount_:nat)
      : (k,v) ledger_module =
  let balance_ = get_for_user ledger_module from_ token_id in
  let balance_ = ledger_module.sub_to_val from_ amount_ balance_ in
  let ledger   = set_for_user ledger_module from_ token_id balance_ in
  ledger

let increase_token_amount_for_user 
      (type k v) 
      (ledger_module: (k,v) ledger_module) 
      (to_: owner) 
      (token_id: Token.t)
      (amount_:nat)
      : (k,v) ledger_module =
  let balance_ = get_for_user ledger_module to_ token_id in
  let balance_ = ledger_module.add_to_val to_ amount_ balance_ in
  let ledger   = set_for_user ledger_module to_ token_id balance_ in
  ledger

(* Possible types as defined in the TZIP-12 *)

type ledger = t

module Single_asset = struct 
  type k = address
  type v = Amount.t
  type t = (k,v) ledger
  let make_key (a:address) (_: Token.t) = a
  let balance_of (_:address) (value:v option) = 
      match value with None -> 0n | Some v -> v 
  let add_to_val (address:address) (amount_:Amount.t) (old_value:v option) = 
      let old_value = balance_of address old_value in
      Some (old_value + amount_)
  let sub_to_val (address:address) (amount_:Amount.t) (old_value:v option) = 
      let old_value = balance_of address old_value in
      let () = assert_with_error (old_value >= amount_) Errors.ins_balance in
      Some (abs (old_value - amount_))
  let ledger_module (data: (k,v) ledger) : (k,v) ledger_module = { 
        data; make_key; add_to_val; sub_to_val; balance_of 
      }
end

module Multi_asset = struct
  type k = address * Token.t
  type v = Amount.t
  type t = (k,v) t
  let make_key (a:address) (t: Token.t) = a,t
  let balance_of (_:address) (value:v option) = 
      match value with None -> 0n | Some v -> v 
  let add_to_val (address:address) (amount_:Amount.t) (old_value:v option) = 
      let old_value = balance_of address old_value in
      Some (old_value + amount_)
  let sub_to_val (address:address) (amount_:Amount.t) (old_value:v option) = 
      let old_value = balance_of address old_value in
      let () = assert_with_error (old_value >= amount_) Errors.ins_balance in
      Some (abs (old_value - amount_))
  let ledger_module (data: (k,v) ledger) : (k,v) ledger_module = { 
        data; make_key; add_to_val; sub_to_val; balance_of 
      }
end

module NFT = struct
  type k = Token.t
  type v = address * Amount.t
  type t = (k,v) t
  let make_key (_address:address) (t: Token.t) = t
  let balance_of (address:address) (value:v option) = 
      match value with None -> 0n | Some (own,amount) -> if (own = address) then amount else 0n
  let add_to_val (address:address) (amount:Amount.t) (_old_value:v option) = 
        if amount = 1n then Some (address,amount) else failwith Errors.amount_net_expected
  let sub_to_val (_address:address) (amount :Amount.t) (_old_value:v option) = 
        if amount = 1n then (None:v option) else failwith Errors.amount_net_expected        
  let ledger_module (data: (k,v) ledger) : (k,v) ledger_module = { 
        data; make_key; add_to_val; sub_to_val; balance_of 
      }
end
