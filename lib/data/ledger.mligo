#import "errors.mligo" "Errors"
#import "token.mligo" "Token"
#import "amount.mligo" "Amount"

type owner = address

type 'l ledger_module = { 
    data      : 'l; 
    empty     : 'l;
    increase  : 'l * owner * Token.t * Amount.t -> 'l;
    decrease  : 'l * owner * Token.t * Amount.t -> 'l;
    balance_of: 'l * owner * Token.t -> Amount.t;
    supply    : 'l * Token.t -> Amount.t
}

[@inline]
let decrease_token_amount_for_user 
      (type l) 
      (ledger_module: l ledger_module) 
      (from_: owner) 
      (token_id: Token.t) 
      (amount_:nat)
      : l ledger_module =
  let data = ledger_module.decrease (ledger_module.data, from_, token_id, amount_) in
  { ledger_module with data }

[@inline]
let increase_token_amount_for_user 
      (type l) 
      (ledger_module: l ledger_module) 
      (to_: owner) 
      (token_id: Token.t)
      (amount_:nat)
      : l ledger_module =
  let data = ledger_module.increase (ledger_module.data, to_, token_id, amount_) in
  { ledger_module with data }

(* Possible types as defined in the TZIP-12 *)

module Common_Asset = struct 
  let balance_of (value:Amount.t option) = 
      match value with None -> 0n | Some v -> v 

  let add_to_val (amount_:Amount.t) (old_value:Amount.t option) = 
      let old_value = balance_of old_value in
      old_value + amount_

  let sub_to_val (amount_:Amount.t) (old_value:Amount.t option) = 
      let old_value = balance_of old_value in
      let () = assert_with_error (old_value >= amount_) Errors.ins_balance in
      abs (old_value - amount_)
end

module Single_asset = struct 
  type l = {
    values: (address,Amount.t) big_map;
    supply: Amount.t
  }

  let empty : l = { 
    values = Big_map.empty; 
    supply = 0n 
  }

  let get_for_user (ledger: l) (owner: owner) : Amount.t option =
    Big_map.find_opt owner ledger.values

  let set_for_user (ledger: l) (owner: owner) (value: Amount.t): l =
    let values = Big_map.update owner (Some value) ledger.values in
    { ledger with values }

  let get_supply (ledger: l): Amount.t = 
    ledger.supply

  let set_supply (ledger: l) (value: Amount.t): l =
    let supply = value in
    { ledger with supply }

  let decrease (ledger,from_,_token_id,amount_: l * owner * Token.t * Amount.t): l =
    let balance_ = get_for_user ledger from_ in
    let balance_ = Common_Asset.sub_to_val amount_ balance_ in
    let ledger   = set_for_user ledger from_ balance_ in
    let balance_ = Common_Asset.sub_to_val amount_ (Some (get_supply ledger)) in
    let ledger   = set_supply ledger balance_ in
    ledger

  let increase (ledger,to_,_token_id,amount_: l * owner * Token.t * Amount.t): l =
    let balance_ = get_for_user ledger to_ in
    let balance_ = Common_Asset.add_to_val amount_ balance_ in
    let ledger   = set_for_user ledger to_ balance_ in
    let balance_ = Common_Asset.add_to_val amount_ (Some (get_supply ledger)) in
    let ledger   = set_supply ledger balance_ in
    ledger

  let balance_of (ledger,owner,_token_id: l * owner * Token.t): Amount.t =
    let value = get_for_user ledger owner in
    Common_Asset.balance_of value

  let supply (ledger,_:l * Token.t) = 
    ledger.supply

  let ledger_module (data: l) : l ledger_module = { 
    data; empty; increase; decrease; balance_of; supply
  }
end

module Multi_asset = struct 
  type l = {
    values: ((address * Token.t),Amount.t) big_map;
    supply: (Token.t, Amount.t) map
  }

  let empty : l = { 
    values = Big_map.empty; 
    supply = Map.empty 
  }

  let make_key (a:address) (t: Token.t) = (a,t)

  let get_for_user (ledger: l) (owner: owner) (token_id: Token.t): Amount.t option =
    let key = make_key owner token_id in
    Big_map.find_opt key ledger.values

  let set_for_user (ledger: l) (owner: owner) (token_id: Token.t) (value: Amount.t): l =
    let key = make_key owner token_id in
    let values = Big_map.update key (Some value) ledger.values in
    { ledger with values }

  let get_supply (ledger: l) (token_id: Token.t): Amount.t =
    match Map.find_opt token_id ledger.supply with
    | None -> 0n
    | Some n -> n

  let set_supply (ledger: l) (token_id: Token.t) (value: Amount.t): l =
    let supply = Map.update token_id (Some value) ledger.supply in
    { ledger with supply }

  let decrease (ledger,from_,token_id,amount_: l * owner * Token.t * Amount.t): l =
    let balance_ = get_for_user ledger from_ token_id in
    let balance_ = Common_Asset.sub_to_val amount_ balance_ in
    let ledger   = set_for_user ledger from_ token_id balance_ in
    let balance_ = Common_Asset.sub_to_val amount_ (Some (get_supply ledger token_id)) in
    let ledger   = set_supply ledger token_id balance_ in
    ledger

  let increase (ledger,to_,token_id,amount_: l * owner * Token.t * Amount.t): l =
    let balance_ = get_for_user ledger to_ token_id in
    let balance_ = Common_Asset.add_to_val amount_ balance_ in
    let ledger   = set_for_user ledger to_ token_id balance_ in
    let balance_ = Common_Asset.add_to_val amount_ (Some (get_supply ledger token_id)) in
    let ledger   = set_supply ledger token_id balance_ in
    ledger

  let balance_of (ledger,owner,token_id: l * owner * Token.t): Amount.t =
    let value = get_for_user ledger owner token_id in
    Common_Asset.balance_of value

  let supply (ledger,token_id: l * Token.t) =
    get_supply ledger token_id

  let ledger_module (data: l) : l ledger_module = { 
    data; empty; increase; decrease; balance_of; supply
  }
end

module NFT = struct 
  type l = (Token.t,address) big_map

  let empty = (Big_map.empty : l)

  let balance_of (address:address) (value:owner option) = 
    match value with None -> 0n | Some own -> if (own = address) then 1n else 0n

  let add_to_val (address:address) (amount:Amount.t) = 
    if amount = 1n then Some address else failwith Errors.amount_net_expected

  let sub_to_val (amount :Amount.t) = 
    if amount = 1n then (None:owner option) else failwith Errors.amount_net_expected        

  let get_for_user (ledger: l) (token_id: Token.t): owner option =
    Big_map.find_opt token_id ledger

  let set_for_user (ledger: l) (owner: owner) (token_id: Token.t): l =
    Big_map.update token_id (Some owner) ledger

  let decrease (ledger,from_,token_id,amount_: l * owner * Token.t * Amount.t): l =
    let _        = get_for_user ledger token_id in
    let _        = sub_to_val amount_ in
    let ledger   = set_for_user ledger from_ token_id in
    ledger

  let increase (ledger,to_,token_id,amount_: l * owner * Token.t * Amount.t): l =
    let _      = get_for_user ledger token_id in
    let _      = add_to_val to_ amount_ in
    let ledger = set_for_user ledger to_ token_id in
    ledger

  let balance_of (ledger,owner,token_id: l * owner * Token.t): Amount.t =
    let value = get_for_user ledger token_id in
    balance_of owner value

  let supply (ledger, token_id: l * Token.t): Amount.t =
    match Big_map.find_opt token_id ledger with
    | Some _ -> 1n
    | None   -> 0n

  let ledger_module (data: l) : l ledger_module = { 
    data; empty; increase; decrease; balance_of; supply
  }
end
