#import "../data/token.mligo" "Token"
#import "../data/ledger.mligo" "Ledger"
#import "../data/operators.mligo" "Operators"
#import "../data/storage.mligo" "Storage"

type storage = Storage.t
type ledger_module = Ledger.ledger_module

let balance_of 
         (type a l) 
         (make: l -> l ledger_module) 
         ((owner, token_id), storage : (address * Token.t) * (a,l) storage) 
         : nat =
   let ledger_module = make storage.ledger in
   ledger_module.balance_of (ledger_module.data, owner, token_id)

let total_supply 
         (type a l) 
         ((token_id, storage) : (nat * (a,l) storage)) 
         : nat =
   if Storage.token_exist storage token_id 
   then 1n 
   else 0n
