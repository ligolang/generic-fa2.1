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
#import "../entrypoints/export_ticket.mligo" "Export_ticket"
#import "../entrypoints/import_ticket.mligo" "Import_ticket"

#import "views.mligo" "Views"

type storage = Storage.t

type ledger = Ledger.t
type ledger_module = Ledger.ledger_module

type 'a parameter = [@layout:comb]
   | Transfer         of Transfer.transfer
   | Balance_of       of Balance_of.balance_of
   | Update_operators of Update.update_operators
   | Extension        of 'a

let main 
         (type p a k v) 
         (make:(k,v) ledger -> (k,v) ledger_module) 
         (extension:p -> (a,k,v) storage -> (k,v) ledger_module -> operation list * (a,k,v) storage)
         ((p,s):(p parameter * (a,k,v) storage)) 
         : operation list * (a,k,v) storage = 
   match p with
   | Transfer         p -> Transfer.transfer p s (make s.ledger)
   | Balance_of       p -> Balance_of.balance_of p s (make s.ledger)
   | Update_operators p -> Update.update_ops p s
   | Extension        p -> extension p s (make s.ledger)

