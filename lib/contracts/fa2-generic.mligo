#import "../data/errors.mligo" "Errors"
#import "../data/metadata.mligo" "Metadata"
#import "../data/token.mligo" "Token"
#import "../data/ledger.mligo" "Ledger"
#import "../data/operators.mligo" "Operators"
#import "../data/approvals.mligo" "Approvals"
#import "../data/tokenMetadata.mligo" "TokenMetadata"
#import "../data/storage.mligo" "Storage"

#import "../entrypoints/transfer.mligo" "Transfer"
#import "../entrypoints/balance_of.mligo" "Balance_of"
#import "../entrypoints/update.mligo" "Update"
#import "../entrypoints/approve.mligo" "Approve"

#import "views.mligo" "Views"

type storage = Storage.t

type ledger_module = Ledger.ledger_module

type 'a parameter = [@layout:comb]
   | Transfer         of Transfer.transfer
   | Balance_of       of Balance_of.balance_of
   | Update_operators of Update.update_operators
   | Extension        of 'a

let main 
         (type p a l) 
         (make:l -> l ledger_module) 
         (extension:p -> (a,l) storage -> l ledger_module -> operation list * (a,l) storage)
         ((p,s):(p parameter * (a,l) storage)) 
         : operation list * (a,l) storage = 
   match p with
   | Transfer         p -> Transfer.transfer p s (make s.ledger)
   | Balance_of       p -> Balance_of.balance_of p s (make s.ledger)
   | Update_operators p -> Update.update_ops p s
   | Extension        p -> extension p s (make s.ledger)

