#import "../../data/token.mligo" "Token"
#import "../../data/amount.mligo" "Amount"

type approve = [@layout:comb] {
    owner     : address;
    spender   : address;
    token_id  : Token.t;
    old_value : nat;
    new_value : nat;
} 

type approvals = approve list

type approval_event = [@layout:comb] {
    sender         : address;
    approval_update: approvals; 
}

let make_event (approval_update:approvals): operation = 
    let sender = Tezos.get_sender () in 
    Tezos.emit "%approval_event" { sender; approval_update }
