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

#import "./fa2.1-generic.mligo" "FA2_1"

type parametric_parameter = FA2_1.parameter
type parametric_storage = Storage.t

type parameter = unit parametric_parameter
type storage = (unit, Ledger.Single_asset.l) parametric_storage

let main : parameter * storage -> operation list * storage = 
   FA2_1.main Ledger.Single_asset.ledger_module (fun _ s _ -> [],s)

(*
   Views corner
*)

[@view] let balance_of : ((address * Token.t) * storage) -> nat =
   FA2_1.Views.balance_of Ledger.Single_asset.ledger_module

[@view] let total_supply : (Token.t * storage) -> nat =
   FA2_1.Views.total_supply Ledger.Single_asset.ledger_module
