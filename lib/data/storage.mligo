#import "errors.mligo" "Errors"
#import "metadata.mligo" "Metadata"
#import "token.mligo" "Token"
#import "operators.mligo" "Operators"
#import "approvals.mligo" "Approvals"
#import "tokenMetadata.mligo" "TokenMetadata"
#import "ledger.mligo" "Ledger"

type ledger = Ledger.t

type ('a,'k,'v) t = {
  metadata: Metadata.t;
  ledger: ('k,'v) ledger;
  token_metadata: TokenMetadata.t;
  operators: Operators.t option;
  approvals: Approvals.t; 
  extension: 'a;
}

let token_exist (type a k v) (s:(a,k,v) t) (token_id : nat) : bool  = 
    match Big_map.find_opt token_id s.token_metadata with
    | Some _ -> true
    | None   -> false

let assert_token_exist (type a k v) (s:(a,k,v) t) (token_id : Token.t) : unit  =
    if not (token_exist s token_id)
    then failwith Errors.undefined_token

let get_ledger (type a k v) (s:(a,k,v) t) = 
    s.ledger

let set_ledger (type a k v) (s:(a,k,v) t) (ledger: (k,v) ledger) = 
    {s with ledger = ledger}

let get_operators (type a k v) (s:(a,k,v) t) = 
    s.operators

let set_operators (type a k v) (s:(a,k,v) t) (operators:Operators.t) = 
    {s with operators = Some operators}

let get_approvals (type a k v) (s:(a,k,v) t) = 
    s.approvals

let set_approvals (type a k v) (s:(a,k,v) t) (approvals:Approvals.t) =
    {s with approvals = approvals}
