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

#import "./fa2-generic.mligo" "FA2"

type parametric_parameter = FA2.parameter
type parametric_storage = Storage.t

type parameter = unit parametric_parameter
type storage = (unit, Ledger.Multi_asset.l) parametric_storage

let main : parameter -> storage -> operation list * storage = 
   FA2.main Ledger.Multi_asset.ledger_module (fun _ s _ -> [], s)

(*
   Views corner
*)

[@view] let balance_of : ((address * Token.t) * storage) -> nat =
   FA2.Views.balance_of Ledger.Multi_asset.ledger_module 

[@view] let total_supply : (Token.t * storage) -> nat =
   FA2.Views.total_supply Ledger.Multi_asset.ledger_module 
