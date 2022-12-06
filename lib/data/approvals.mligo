#import "errors.mligo" "Errors"
#import "token.mligo" "Token"
#import "amount.mligo" "Amount"

type owner    = address
type spender  = address

type t = ((owner * spender * Token.t), Amount.t) big_map

let get_amount 
      (approvals:t) 
      (owner: owner) 
      (spender: spender)
      (token_id: Token.t) 
      : Amount.t =
  match Big_map.find_opt (owner, spender, token_id) approvals with 
    Some (a) -> a 
  | None -> 0n

let set_amount 
      (approvals:t) 
      (owner: owner) 
      (spender: spender) 
      (token_id: Token.t) 
      (amount_: Amount.t)
      : t =
  Big_map.update (owner,spender,token_id) (Some amount_) approvals

let decrease_approved_amount 
      (approvals: t) 
      (from_: owner) 
      (spender: spender) 
      (token_id: Token.t) 
      (amount_: Amount.t): t =
  let balance_  = get_amount approvals from_ spender token_id in
  let ()        = assert_with_error (balance_ >= amount_) Errors.ins_balance in
  let balance_  = abs (balance_ - amount_) in
  let approvals = set_amount approvals from_ spender token_id balance_ in
  approvals

let increase_approved_amount 
      (approvals: t) 
      (to_: owner) 
      (spender: spender) 
      (token_id: Token.t) 
      (amount_: Amount.t): t =
  let balance_  = get_amount approvals to_ spender token_id in
  let balance_  = balance_ + amount_ in
  let approvals = set_amount approvals to_ spender token_id balance_ in
  approvals
