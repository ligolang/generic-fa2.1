#import "../data/token.mligo" "Token"
#import "../data/ledger.mligo" "Ledger"
#import "../data/operators.mligo" "Operators"
#import "../data/storage.mligo" "Storage"

type storage = Storage.t
type ledger = Ledger.t
type ledger_module = Ledger.ledger_module

let balance_of 
         (type a k v) 
         (make:(k,v) ledger -> (k,v) ledger_module) 
         ((owner, token_id), storage : (address * Token.t) * (a,k,v) storage) 
         : nat =
   let ledger_module = make storage.ledger in
   let value = Ledger.get_for_user ledger_module owner token_id in
   ledger_module.balance_of owner value

let total_supply 
         (type a k v) 
         ((token_id, storage) : (nat * (a,k,v) storage)) 
         : nat =
   if Storage.token_exist storage token_id 
   then 1n 
   else 0n
