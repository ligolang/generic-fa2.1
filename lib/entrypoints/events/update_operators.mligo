#import "../../data/token.mligo" "Token"
#import "../../data/amount.mligo" "Amount"

type operator_update = [@layout:comb] {
    owner       : address;
    operator    : address;
    token_id    : Token.t;
    is_operator : bool;
} 

type operator_update_event = [@layout:comb] {
    sender         : address;
    operator_update: operator_update list; 
}

let make_update (owner:address) (operator:address) (token_id:Token.t) (is_operator:bool): operator_update  =
    { owner; operator; token_id; is_operator }

let make_event (operator_update:operator_update list): operation = 
    let sender = Tezos.get_sender () in 
    Tezos.emit "%operator_update_event" { sender; operator_update }
