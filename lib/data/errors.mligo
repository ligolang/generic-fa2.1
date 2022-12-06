type t = string

let undefined_token = "FA2_TOKEN_UNDEFINED"
let ins_balance     = "FA2_INSUFFICIENT_BALANCE"
let no_transfer     = "FA2_TX_DENIED"
let not_owner       = "FA2_NOT_OWNER"
let not_operator    = "FA2_NOT_OPERATOR"
let not_supported   = "FA2_OPERATORS_UNSUPPORTED"
let rec_hook_fail   = "FA2_RECEIVER_HOOK_FAILED"
let send_hook_fail  = "FA2_SENDER_HOOK_FAILED"
let rec_hook_undef  = "FA2_RECEIVER_HOOK_UNDEFINED"
let send_hook_under = "FA2_SENDER_HOOK_UNDEFINED"

let invalid_ticket  = "FA2.1_INVALID_TICKET"
let unsafe_approval = "FA2.1_UNSAFE_APPROVAL_CHANGE"

let cannot_create_ticket = "Ticket cannot be created (amount is problably 0)"
let storage_has_no_operators = "The storage does not support operators management"
let only_sender_manage_operators = "The sender can only manage operators for his own token"
let amount_net_expected = "NFT transaction amount should be 1n"