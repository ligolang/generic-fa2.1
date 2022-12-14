#import "../data/token.mligo" "Token"
#import "../data/amount.mligo" "Amount"
#import "../data/ledger.mligo" "Ledger"
#import "../data/storage.mligo" "Storage"

type storage = Storage.t

type request = {
      owner    : address;
      token_id : Token.t;
   }

type callback = [@layout:comb] {
      request : request;
      balance : Amount.t;
   }

type balance_of = [@layout:comb] {
      requests : request list;
      callback : callback list contract;
   }

type t = balance_of
type ledger_module = Ledger.ledger_module

let get_balance_info 
         (type a l) 
         (storage: (a, l) storage) 
         (ledger_module: l ledger_module) 
         (request : request) 
         : callback =
   let {owner;token_id} = request in
   let () = Storage.assert_token_exist storage token_id in
   let balance_ = ledger_module.balance_of (ledger_module.data, owner, token_id) in
   {request=request;balance=balance_}

let balance_of 
         (type a l) 
         (balance: balance_of) 
         (storage: (a,l) storage) 
         (ledger_module: l ledger_module) 
         : operation list * (a, l) storage =
   let {requests;callback} = balance in
   let callback_param = List.map (get_balance_info storage ledger_module) requests in
   let operation = Tezos.transaction callback_param 0tez callback in
   [ operation ], storage
