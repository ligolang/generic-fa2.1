(* with TZIP-12 *)
type data = {
    token_id  : nat;
    token_info: (string,bytes)map
}

type t = (nat, data) big_map

(**
    This should be initialized at origination, conforming to either:
    TZIP-12 : https://gitlab.com/tezos/tzip/-/blob/master/proposals/tzip-12/tzip-12.md#token-metadata
    or 
    TZIP-16 : https://gitlab.com/tezos/tzip/-/blob/master/proposals/tzip-12/tzip-12.md#contract-metadata-tzip-016
*)

let data1 = Map.literal [
    ("name", [%bytes {| "FA2 multi asset 1" |}]);
    ("symbol", [%bytes {| "FMA1" |}]);
    ("decimals", [%bytes {| "3" |}]);
]

let data3 = Map.literal [
    ("name", [%bytes {| "FA2 multi asset 3" |}]);
    ("symbol", [%bytes {| "FMA3" |}]);
    ("decimals", [%bytes {| "3" |}]);
]


let init () : t = Big_map.literal [
    (1n, {token_id=1n;token_info=data1});
    (3n, {token_id=3n;token_info=data3});
]