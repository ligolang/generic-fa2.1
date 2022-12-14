#import "errors.mligo" "Errors"
#import "metadata.mligo" "Metadata"
#import "token.mligo" "Token"
#import "operators.mligo" "Operators"
#import "approvals.mligo" "Approvals"
#import "tokenMetadata.mligo" "TokenMetadata"
#import "ledger.mligo" "Ledger"

type ('a,'l) t = {
  metadata: Metadata.t;
  ledger: 'l;
  token_metadata: TokenMetadata.t;
  operators: Operators.t option;
  approvals: Approvals.t; 
  extension: 'a;
}

let token_exist (type a l) (s:(a,l) t) (token_id : nat) : bool  = 
    Big_map.mem token_id s.token_metadata

let assert_token_exist (type a l) (s:(a,l) t) (token_id : Token.t) : unit  =
    if not (token_exist s token_id)
    then failwith Errors.undefined_token

let get_ledger (type a l) (s:(a,l) t) = 
    s.ledger

let set_ledger (type a l) (s:(a,l) t) (ledger: l) = 
    {s with ledger = ledger}

let get_operators (type a l) (s:(a,l) t) = 
    s.operators

let set_operators (type a l) (s:(a,l) t) (operators:Operators.t) = 
    {s with operators = Some operators}

let get_approvals (type a l) (s:(a,l) t) = 
    s.approvals

let set_approvals (type a l) (s:(a,l) t) (approvals:Approvals.t) =
    {s with approvals = approvals}
