#import "../lib/contracts/fa2-NFT.mligo" "FA2_NFT"
#import "helpers/list.mligo" "List_helper"

module Callback = struct
  type storage = nat list

  type request = {
    owner    : address;
    token_id : nat;
  }

  type callback = [@layout:comb] {
    request : request;
    balance : nat;
  }

  type parameter = callback list

  let main (responses:parameter) (_:storage) =
    let balances = List.map (fun (r : callback) -> r.balance) responses in
    ([]: operation list), balances
end

let get_initial_storage () = 
  let () = Test.reset_state 8n ([
    1000000tez;
    1000000tez;
    1000000tez;
    1000000tez;
    1000000tez;
    1000000tez;
    1000000tez;
    1000000tez;
  ] : tez list) in

  let baker = Test.nth_bootstrap_account 7 in 
  let () = Test.set_baker baker in

  let owner1 = Test.nth_bootstrap_account 0 in 
  let owner2 = Test.nth_bootstrap_account 1 in 
  let owner3 = Test.nth_bootstrap_account 2 in 
  let owner4 = Test.nth_bootstrap_account 6 in 

  let owners = [owner1; owner2; owner3; owner4] in

  let op1 = Test.nth_bootstrap_account 3 in
  let op2 = Test.nth_bootstrap_account 4 in
  let op3 = Test.nth_bootstrap_account 5 in

  let ops = [op1; op2; op3] in
    let ledger = Big_map.literal ([
    (1n, owner1);
    (2n, owner2);
    (3n, owner3);
    (4n, owner4);
    (5n, owner4);
  ])
  in

  let operators = Big_map.literal ([
    ((owner1, 1n), Set.literal [op1]);
    ((owner2, 2n), Set.literal [op1]);
    ((owner3, 3n), Set.literal [op1]);
    ((op1   , 1n), Set.literal [op3]);
    ((owner4, 4n), Set.literal [op1]);
    ((owner4, 5n), Set.literal [op1]);
  ])
  in
  
  let token_metadata = (Big_map.literal [
    (1n, ({token_id=1n;token_info=(Map.empty : (string, bytes) map);} : FA2_NFT.TokenMetadata.data));
    (2n, ({token_id=2n;token_info=(Map.empty : (string, bytes) map);} : FA2_NFT.TokenMetadata.data));
    (3n, ({token_id=3n;token_info=(Map.empty : (string, bytes) map);} : FA2_NFT.TokenMetadata.data));
    (4n, ({token_id=3n;token_info=(Map.empty : (string, bytes) map);} : FA2_NFT.TokenMetadata.data));
    (5n, ({token_id=3n;token_info=(Map.empty : (string, bytes) map);} : FA2_NFT.TokenMetadata.data));
  ] : FA2_NFT.TokenMetadata.t) in

  let approvals = (Big_map.empty : FA2_NFT.Approvals.t)
  in

  let metadata = FA2_NFT.Metadata.init() in
  let _token_ids = Set.literal [1n; 2n; 3n] in

  let initial_storage = {
    ledger         = ledger;
    token_metadata = token_metadata;
    operators      = Some operators;
    // token_ids      = token_ids;
    metadata       = metadata;
    approvals      = approvals;
    extension      = ()
  } in

  (initial_storage:FA2_NFT.storage), owners, ops


let assert_balances 
  (contract_address : (FA2_NFT.parameter, FA2_NFT.storage) typed_address ) 
  (a, b, c : (address * nat) * (address * nat) * (address * nat)) = 
  let (owner1, token_id_1) = a in
  let (owner2, token_id_2) = b in
  let (owner3, token_id_3) = c in
  let storage = Test.get_storage contract_address in
  let ledger = storage.ledger in
  let () = match (Big_map.find_opt token_id_1 ledger) with
    Some amt -> assert (amt = owner1)
  | None -> Test.failwith "incorret address" 
  in
  let () = match (Big_map.find_opt token_id_2 ledger) with
    Some amt ->  assert (amt = owner2)
  | None -> Test.failwith "incorret address" 
  in
  let () = match (Big_map.find_opt token_id_3 ledger) with
    Some amt -> assert (amt = owner3)
  | None -> Test.failwith "incorret address" 
  in
  ()

let assert_error (result : test_exec_result) (error : FA2_NFT.Errors.t) =
  match result with
    Success _ -> Test.failwith "This test should fail"
  | Fail (Rejected (err, _))  -> assert (Test.michelson_equal err (Test.eval error))
  | Fail _ -> Test.failwith "invalid test failure"

(* Tests for FA2 NFT contract *)

type return = operation list * FA2_NFT.storage
type main_fn = FA2_NFT.parameter -> FA2_NFT.storage -> return

(* Transfer *)

(* 1. transfer successful *)
let _test_atomic_tansfer_operator_success (main : main_fn) =
  let initial_storage, owners, operators = get_initial_storage () in
  let owner1 = List_helper.nth_exn 0 owners in
  let owner2 = List_helper.nth_exn 1 owners in
  let owner3 = List_helper.nth_exn 2 owners in
  let op1    = List_helper.nth_exn 0 operators in
  let transfer_requests = [
    {from_=owner1; txs=[{to_=owner2;amount=1n;token_id=1n};]};
  ]
  in
  let () = Test.set_source op1 in 
  let (t_addr,_,_) = Test.originate main initial_storage 0tez in
  let contr = Test.to_contract t_addr in
  let _ = Test.transfer_to_contract_exn contr (Transfer transfer_requests) 0tez in
  let () = assert_balances t_addr ((owner2, 1n), (owner2, 2n), (owner3, 3n)) in
  ()
let test_atomic_tansfer_operator_success = _test_atomic_tansfer_operator_success FA2_NFT.main

(* 1.1. transfer successful owner *)
let _test_atomic_tansfer_owner_success (main : main_fn) =
  let initial_storage, owners, _ = get_initial_storage () in
  let owner1 = List_helper.nth_exn 0 owners in
  let owner2 = List_helper.nth_exn 1 owners in
  let owner3 = List_helper.nth_exn 2 owners in
  let transfer_requests = [
    {from_=owner1; txs=[{to_=owner2;amount=1n;token_id=1n};]};
  ]
  in
  let () = Test.set_source owner1 in 
  let (t_addr,_,_) = Test.originate main initial_storage 0tez in
  let contr = Test.to_contract t_addr in
  let _ = Test.transfer_to_contract_exn contr (Transfer transfer_requests) 0tez in
  let () = assert_balances t_addr ((owner2, 1n), (owner2, 2n), (owner3, 3n)) in
  ()
let test_atomic_tansfer_owner_success = _test_atomic_tansfer_owner_success FA2_NFT.main

(* 2. transfer failure token undefined *)
let _test_transfer_token_undefined (main : main_fn) = 
  let initial_storage, owners, operators = get_initial_storage () in
  let owner1 = List_helper.nth_exn 0 owners in
  let owner2 = List_helper.nth_exn 1 owners in
  let op1    = List_helper.nth_exn 0 operators in
  let transfer_requests = [
    {from_=owner1; txs=[{to_=owner2;amount=1n;token_id=15n};]};
  ]
  in
  let () = Test.set_source op1 in 
  let (t_addr,_,_) = Test.originate main initial_storage 0tez in
  let contr = Test.to_contract t_addr in
  let result = Test.transfer_to_contract contr (Transfer transfer_requests) 0tez in
  assert_error result FA2_NFT.Errors.undefined_token
let test_transfer_token_undefined = _test_transfer_token_undefined FA2_NFT.main

(* 3. transfer failure incorrect operator *)
let _test_atomic_transfer_failure_not_operator (main : main_fn) = 
  let initial_storage, owners, operators = get_initial_storage () in
  let owner1 = List_helper.nth_exn 0 owners in
  let owner2 = List_helper.nth_exn 1 owners in
  let op2    = List_helper.nth_exn 1 operators in
  let transfer_requests = [
    {from_=owner1; txs=[{to_=owner2;amount=1n;token_id=1n};]};
  ]
  in
  let () = Test.set_source op2 in 
  let (t_addr,_,_) = Test.originate main initial_storage 0tez in
  let contr = Test.to_contract t_addr in
  let result = Test.transfer_to_contract contr (Transfer transfer_requests) 0tez in
  assert_error result FA2_NFT.Errors.not_operator
let test_atomic_transfer_failure_not_operator 
  = _test_atomic_transfer_failure_not_operator FA2_NFT.main

(* 4. self transfer *)
let _test_atomic_tansfer_success_zero_amount_and_self_transfer (main : main_fn) =
  let initial_storage, owners, operators = get_initial_storage () in
  let owner1 = List_helper.nth_exn 0 owners in
  let owner2 = List_helper.nth_exn 1 owners in
  let owner3 = List_helper.nth_exn 2 owners in
  let op1    = List_helper.nth_exn 0 operators in
  let transfer_requests = [
    {from_=owner2; txs=[{to_=owner2;amount=1n;token_id=2n};]};
  ]
  in
  let () = Test.set_source op1 in 
  let (t_addr,_,_) = Test.originate main initial_storage 0tez in
  let contr = Test.to_contract t_addr in
  let _ = Test.transfer_to_contract_exn contr (Transfer transfer_requests) 0tez in
  let () = assert_balances t_addr ((owner1, 1n), (owner2, 2n), (owner3, 3n)) in
  ()
let test_atomic_tansfer_success_zero_amount_and_self_transfer =
  _test_atomic_tansfer_success_zero_amount_and_self_transfer FA2_NFT.main

(* 5. transfer failure transitive operators *)
let _test_transfer_failure_transitive_operators (main : main_fn) = 
  let initial_storage, owners, operators = get_initial_storage () in
  let owner1 = List_helper.nth_exn 0 owners in
  let owner2 = List_helper.nth_exn 1 owners in
  let op3    = List_helper.nth_exn 2 operators in
  let transfer_requests = [
    {from_=owner1; txs=[{to_=owner2;amount=1n;token_id=1n};]};
  ]
  in
  let () = Test.set_source op3 in 
  let (t_addr,_,_) = Test.originate main initial_storage 0tez in
  let contr = Test.to_contract t_addr in
  let result = Test.transfer_to_contract contr (Transfer transfer_requests) 0tez in
  assert_error result FA2_NFT.Errors.not_operator
let test_transfer_failure_transitive_operators = 
  _test_transfer_failure_transitive_operators FA2_NFT.main

(* Balance of *)

(* 6. empty balance of + callback with empty response *)
let _test_empty_transfer_and_balance_of (main : main_fn) = 
  let initial_storage, _owners, _operators = get_initial_storage () in
  let (callback_addr,_,_) = Test.originate Callback.main ([] : nat list) 0tez in
  let callback_contract = Test.to_contract callback_addr in

  let balance_of_requests = {
    requests = ([] : FA2_NFT.Balance_of.request list);
    callback = callback_contract;
  } in

  let (t_addr,_,_) = Test.originate main initial_storage 0tez in
  let contr = Test.to_contract t_addr in
  let _ = Test.transfer_to_contract_exn contr (Balance_of balance_of_requests) 0tez in

  let callback_storage = Test.get_storage callback_addr in
  assert (callback_storage = ([] : nat list))
let test_empty_transfer_and_balance_of = _test_empty_transfer_and_balance_of FA2_NFT.main

(* 7. balance of failure token undefined *)
let _test_balance_of_token_undefines (main : main_fn) = 
  let initial_storage, owners, operators = get_initial_storage () in
  let owner1 = List_helper.nth_exn 0 owners in
  let owner2 = List_helper.nth_exn 1 owners in
  let _op1   = List_helper.nth_exn 0 operators in
  let (callback_addr,_,_) = Test.originate Callback.main ([] : nat list) 0tez in
  let callback_contract = Test.to_contract callback_addr in

  let balance_of_requests = {
    requests = [
      {owner=owner1;token_id=0n};
      {owner=owner2;token_id=2n};
      {owner=owner1;token_id=1n};
    ];
    callback = callback_contract;
  } in

  let (t_addr,_,_) = Test.originate main initial_storage 0tez in
  let contr = Test.to_contract t_addr in
  let result = Test.transfer_to_contract contr (Balance_of balance_of_requests) 0tez in
  assert_error result FA2_NFT.Errors.undefined_token
let test_balance_of_token_undefines = _test_balance_of_token_undefines FA2_NFT.main

(* 8. duplicate balance_of requests *)
let _test_balance_of_requests_with_duplicates (main : main_fn) = 
  let initial_storage, owners, _ = get_initial_storage () in
  let owner1 = List_helper.nth_exn 0 owners in
  let owner2 = List_helper.nth_exn 1 owners in
  let (callback_addr,_,_) = Test.originate Callback.main ([] : nat list) 0tez in
  let callback_contract = Test.to_contract callback_addr in

  let balance_of_requests = {
    requests = [
      {owner=owner1;token_id=1n};
      {owner=owner2;token_id=2n};
      {owner=owner1;token_id=1n};
      {owner=owner1;token_id=2n};
    ];
    callback = callback_contract;
  } in

  let (t_addr,_,_) = Test.originate main initial_storage 0tez in
  let contr = Test.to_contract t_addr in
  let _ = Test.transfer_to_contract_exn contr (Balance_of balance_of_requests) 0tez in

  let callback_storage = Test.get_storage callback_addr in
  assert (callback_storage = ([1n; 1n; 1n; 0n]))
let test_balance_of_requests_with_duplicates 
  = _test_balance_of_requests_with_duplicates FA2_NFT.main

(* 9. 0 balance if does not hold any tokens (not in ledger) *)
let _test_balance_of_0_balance_if_address_does_not_hold_tokens (main : main_fn) = 
    let initial_storage, owners, operators = get_initial_storage () in
    let owner1 = List_helper.nth_exn 0 owners in
    let owner2 = List_helper.nth_exn 1 owners in
    let op1    = List_helper.nth_exn 0 operators in
    let (callback_addr,_,_) = Test.originate Callback.main ([] : nat list) 0tez in
    let callback_contract = Test.to_contract callback_addr in

    let balance_of_requests = {
      requests = [
        {owner=owner1;token_id=1n};
        {owner=owner2;token_id=2n};
        {owner=op1;token_id=1n};
      ];
      callback = callback_contract;
    } in

    let (t_addr,_,_) = Test.originate main initial_storage 0tez in
    let contr = Test.to_contract t_addr in
    let _ = Test.transfer_to_contract_exn contr (Balance_of balance_of_requests) 0tez in

    let callback_storage = Test.get_storage callback_addr in
    assert (callback_storage = ([1n; 1n; 0n]))
let test_balance_of_0_balance_if_address_does_not_hold_tokens = 
  _test_balance_of_0_balance_if_address_does_not_hold_tokens FA2_NFT.main

(* Update operators *)

(* 10. Remove operator & do transfer - failure *)
let _test_update_operator_remove_operator_and_transfer (main : main_fn) = 
  let initial_storage, owners, operators = get_initial_storage () in
  let owner1 = List_helper.nth_exn 0 owners in
  let owner2 = List_helper.nth_exn 1 owners in
  let op1    = List_helper.nth_exn 0 operators in
  let (t_addr,_,_) = Test.originate main initial_storage 0tez in
  let contr = Test.to_contract t_addr in

  let () = Test.set_source owner1 in 
  let _ = Test.transfer_to_contract_exn contr 
    (Update_operators [
      (Remove_operator {
        owner    = owner1;
        operator = op1;
        token_id = 1n;
      }:FA2_NFT.Update.unit_update)
    ]) 0tez in

  let () = Test.set_source op1 in
  let transfer_requests = [
    {from_=owner1; txs=[{to_=owner2;amount=1n;token_id=1n};]};
  ]
  in
  let result = Test.transfer_to_contract contr (Transfer transfer_requests) 0tez in
  assert_error result FA2_NFT.Errors.not_operator
let test_update_operator_remove_operator_and_transfer = 
  _test_update_operator_remove_operator_and_transfer FA2_NFT.main

(* 10.1. Remove operator & do transfer - failure *)
let _test_update_operator_remove_operator_and_transfer1 (main : main_fn) = 
  let initial_storage, owners, operators = get_initial_storage () in
  let owner4 = List_helper.nth_exn 3 owners in
  let op1    = List_helper.nth_exn 0 operators in
  let (t_addr,_,_) = Test.originate main initial_storage 0tez in
  let contr = Test.to_contract t_addr in

  let () = Test.set_source owner4 in 
  let _ = Test.transfer_to_contract_exn contr 
    (Update_operators [
      (Remove_operator {
        owner    = owner4;
        operator = op1;
        token_id = 4n;
      }:FA2_NFT.Update.unit_update)
    ]) 0tez in

  let storage = Test.get_storage t_addr in
  let operator_tokens = Big_map.find_opt (owner4,5n) (Option.unopt storage.operators) in
  let operator_tokens = Option.unopt operator_tokens in
  assert (operator_tokens = Set.literal [op1])
let test_update_operator_remove_operator_and_transfer1 = 
  _test_update_operator_remove_operator_and_transfer1 FA2_NFT.main


(* 11. Add operator & do transfer - success *)
let _test_update_operator_add_operator_and_transfer (main : main_fn) = 
  let initial_storage, owners, operators = get_initial_storage () in
  let owner1 = List_helper.nth_exn 0 owners in
  let owner2 = List_helper.nth_exn 1 owners in
  let op3    = List_helper.nth_exn 2 operators in
  let (t_addr,_,_) = Test.originate main initial_storage 0tez in
  let contr = Test.to_contract t_addr in

  let () = Test.set_source owner1 in 
  let _ = Test.transfer_to_contract_exn contr 
    (Update_operators [
      (Add_operator {
        owner    = owner1;
        operator = op3;
        token_id = 1n;
      }:FA2_NFT.Update.unit_update)
    ]) 0tez in

  let () = Test.set_source op3 in
  let transfer_requests = [
    {from_=owner1; txs=[{to_=owner2;amount=1n;token_id=1n};]};
  ]
  in
  let _ = Test.transfer_to_contract_exn contr (Transfer transfer_requests) 0tez in
  ()
let test_update_operator_add_operator_and_transfer = 
  _test_update_operator_add_operator_and_transfer FA2_NFT.main

(* 11.1. Add operator & do transfer - success *)
let _test_update_operator_add_operator_and_transfer1 (main : main_fn) = 
  let initial_storage, owners, operators = get_initial_storage () in
  let owner2 = List_helper.nth_exn 1 owners in
  let owner4 = List_helper.nth_exn 3 owners in
  let op3    = List_helper.nth_exn 2 operators in
  let (t_addr,_,_) = Test.originate main initial_storage 0tez in
  let contr = Test.to_contract t_addr in

  let () = Test.set_source owner4 in 
  let _ = Test.transfer_to_contract_exn contr 
    (Update_operators [
      (Add_operator {
        owner    = owner4;
        operator = op3;
        token_id = 4n;
      }:FA2_NFT.Update.unit_update)
    ]) 0tez in

  let () = Test.set_source op3 in
  let transfer_requests = [
    {from_=owner4; txs=[{to_=owner2;amount=1n;token_id=4n};]};
  ]
  in
  let _ = Test.transfer_to_contract_exn contr (Transfer transfer_requests) 0tez in
  ()
let test_update_operator_add_operator_and_transfer1 = 
  _test_update_operator_add_operator_and_transfer1 FA2_NFT.main

let _test_only_sender_manage_operators (main : main_fn) = 
  let initial_storage, owners, operators = get_initial_storage () in
  let owner1 = List_helper.nth_exn 0 owners in
  let owner2 = List_helper.nth_exn 1 owners in
  let op3    = List_helper.nth_exn 2 operators in
  let (t_addr,_,_) = Test.originate main initial_storage 0tez in
  let contr = Test.to_contract t_addr in

  let () = Test.set_source owner2 in 
  let result = Test.transfer_to_contract contr 
    (Update_operators [
      (Add_operator {
        owner    = owner1;
        operator = op3;
        token_id = 1n;
      }:FA2_NFT.Update.unit_update)
    ]) 0tez in

  assert_error result FA2_NFT.Errors.only_sender_manage_operators

let test_only_sender_manage_operators = _test_only_sender_manage_operators FA2_NFT.main