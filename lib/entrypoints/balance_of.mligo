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
type ledger = Ledger.t
type ledger_module = Ledger.ledger_module

let get_balance_info 
         (type a k v) 
         (storage: (a, k, v) storage) 
         (ledger_module: (k,v) ledger_module) 
         (request : request) 
         : callback =
   let {owner;token_id} = request in
   let () = Storage.assert_token_exist storage token_id in
   let value = Ledger.get_for_user ledger_module owner token_id in
   let balance_ = ledger_module.balance_of owner value in
   {request=request;balance=balance_}

let balance_of 
         (type a k v) 
         (balance: balance_of) 
         (storage: (a,k,v) storage) 
         (ledger_module: (k,v) ledger_module) 
         : operation list * (a, k, v) storage =
   let {requests;callback} = balance in
   let callback_param = List.map (get_balance_info storage ledger_module) requests in
   let operation = Tezos.transaction callback_param 0tez callback in
   [ operation ], storage
