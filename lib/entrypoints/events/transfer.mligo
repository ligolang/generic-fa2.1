#import "../../data/token.mligo" "Token"
#import "../../data/amount.mligo" "Amount"

type transaction = {
    to_      : address option;
    token_id : Token.t;
    amount   : Amount.t;
} 

type transfer = {
    from_ : address option;
    txs   : transaction list;
}

type transfer_event = {
    sender  : address;
    transfer: transfer list;
}

let make_transaction (to_: address option) (token_id: Token.t) (amount: Amount.t): transaction = 
    { to_; token_id; amount }

let make_transfer (from_:address option) (txs: transaction list): transfer = 
    { from_; txs}

let make_event (transfer: transfer list): transfer_event =
    let sender = Tezos.get_sender () in
    { sender; transfer }
