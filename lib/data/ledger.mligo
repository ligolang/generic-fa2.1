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
    // supply  : Token.t -> Amount.t
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
      Some (old_value + amount_)

  let sub_to_val (amount_:Amount.t) (old_value:Amount.t option) = 
      let old_value = balance_of old_value in
      let () = assert_with_error (old_value >= amount_) Errors.ins_balance in
      Some (abs (old_value - amount_))
end

module Single_asset = struct 
  type l = (address,Amount.t) big_map

  let empty = (Big_map.empty : l)

  let make_key (a:address) (_: Token.t) = a

  let get_for_user (ledger: l) (owner: owner) (token_id: Token.t): Amount.t option =
    let key = make_key owner token_id in
    Big_map.find_opt key ledger

  let set_for_user (ledger: l) (owner: owner) (token_id: Token.t) (value: Amount.t option): l =
    let key = make_key owner token_id in
    Big_map.update key value ledger

  let decrease (ledger,from_,token_id,amount_: l * owner * Token.t * Amount.t): l =
    let balance_ = get_for_user ledger from_ token_id in
    let balance_ = Common_Asset.sub_to_val amount_ balance_ in
    let ledger   = set_for_user ledger from_ token_id balance_ in
    ledger

  let increase (ledger,to_,token_id,amount_: l * owner * Token.t * Amount.t): l =
    let balance_ = get_for_user ledger to_ token_id in
    let balance_ = Common_Asset.add_to_val amount_ balance_ in
    let ledger   = set_for_user ledger to_ token_id balance_ in
    ledger

  let balance_of (ledger,owner,token_id: l * owner * Token.t): Amount.t =
    let value = get_for_user ledger owner token_id in
    Common_Asset.balance_of value

  let ledger_module (data: l) : l ledger_module = { 
    data; empty; increase; decrease; balance_of
  }
end

module Multi_asset = struct 
  type l = ((address * Token.t),Amount.t) big_map

  let empty = (Big_map.empty : l)

  let make_key (a:address) (t: Token.t) = (a,t)

  let get_for_user (ledger: l) (owner: owner) (token_id: Token.t): Amount.t option =
    let key = make_key owner token_id in
    Big_map.find_opt key ledger

  let set_for_user (ledger: l) (owner: owner) (token_id: Token.t) (value: Amount.t option): l =
    let key = make_key owner token_id in
    Big_map.update key value ledger

  let decrease (ledger,from_,token_id,amount_: l * owner * Token.t * Amount.t): l =
    let balance_ = get_for_user ledger from_ token_id in
    let balance_ = Common_Asset.sub_to_val amount_ balance_ in
    let ledger   = set_for_user ledger from_ token_id balance_ in
    ledger

  let increase (ledger,to_,token_id,amount_: l * owner * Token.t * Amount.t): l =
    let balance_ = get_for_user ledger to_ token_id in
    let balance_ = Common_Asset.add_to_val amount_ balance_ in
    let ledger   = set_for_user ledger to_ token_id balance_ in
    ledger

  let balance_of (ledger,owner,token_id: l * owner * Token.t): Amount.t =
    let value = get_for_user ledger owner token_id in
    Common_Asset.balance_of value

  let ledger_module (data: l) : l ledger_module = { 
    data; empty; increase; decrease; balance_of
  }
end

module NFT = struct 
  type l = (Token.t,address) big_map

  let empty = (Big_map.empty : l)

  let make_key (_:address) (t: Token.t) = t

  let balance_of (address:address) (value:owner option) = 
      match value with None -> 0n | Some own -> if (own = address) then 1n else 0n

  let add_to_val (address:address) (amount:Amount.t) (_old_value:owner option) = 
        if amount = 1n then Some address else failwith Errors.amount_net_expected

  let sub_to_val (_address:address) (amount :Amount.t) (_old_value:owner option) = 
        if amount = 1n then (None:owner option) else failwith Errors.amount_net_expected        

  let get_for_user (ledger: l) (owner: owner) (token_id: Token.t): owner option =
    let key = make_key owner token_id in
    Big_map.find_opt key ledger

  let set_for_user (ledger: l) (owner: owner) (token_id: Token.t) (_value: owner option): l =
    let key = make_key owner token_id in
    Big_map.update key (Some owner) ledger

  let decrease (ledger,from_,token_id,amount_: l * owner * Token.t * Amount.t): l =
    let balance_ = get_for_user ledger from_ token_id in
    let balance_ = sub_to_val from_ amount_ balance_ in
    let ledger   = set_for_user ledger from_ token_id balance_ in
    ledger

  let increase (ledger,to_,token_id,amount_: l * owner * Token.t * Amount.t): l =
    let balance_ = get_for_user ledger to_ token_id in
    let balance_ = add_to_val to_ amount_ balance_ in
    let ledger   = set_for_user ledger to_ token_id balance_ in
    ledger

  let balance_of (ledger,owner,token_id: l * owner * Token.t): Amount.t =
    let value = get_for_user ledger owner token_id in
    balance_of owner value

  let ledger_module (data: l) : l ledger_module = { 
    data; empty; increase; decrease; balance_of
  }
end