#import "../../data/token.mligo" "Token"
#import "../../data/amount.mligo" "Amount"

type approve = {
    owner     : address;
    spender   : address;
    token_id  : Token.t;
    old_value : nat;
    new_value : nat;
} 

type approvals = approve list

type approval_event = {
    sender         : address;
    approval_update: approvals; 
}

let make_event (approval_update:approvals): approval_event = 
    let sender = Tezos.get_sender () in 
    { sender; approval_update }
