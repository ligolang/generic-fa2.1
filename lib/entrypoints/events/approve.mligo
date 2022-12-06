#import "../../data/token.mligo" "Token"
#import "../../data/amount.mligo" "Amount"

type approve = {
    owner     : address;
    spender   : address;
    token_id  : Token.t;
    old_value : nat;
    new_value : nat;
} 

type approvements = approve list

type t = {
    sender         : address;
    operator_update: approvements; 
}

let make_event (operator_update:approvements) = 
    let sender = Tezos.get_sender () in 
    { sender; operator_update }
