# Generic FA2 and FA2.1

This module is a implementation of the last [FA2.1 proposition](https://hackmd.io/eOQdbL1MRlW62M6l6Tjp1Q#).

The current implementation covers:
- multi-asset,
- single-asset and
- NFT.

## Constraints

The code has been designed for the protocol LIMA. 

## Extending the contract

### Adding information to the storage

An extension for a given kind of contract can be done seamlessly. For instance, you want to add a 
list of token identifiers for the NFT this can be done just specifying the extension:

```ligolang
type parametric_parameter = FA2.parameter
type parametric_storage = Storage.t

type extension = Token.t set

type parameter = unit parametric_parameter
type storage = (extension, Ledger.NFT.k, Ledger.NFT.v) parametric_storage

let main : parameter * storage -> operation list * storage = 
   FA2.main Ledger.NFT.ledger_module (fun _ s _ -> [],s)

(*
   Views corner
*)

[@view] let balance_of : ((address * Token.t) * storage) -> nat =
   FA2.Views.balance_of Ledger.NFT.ledger_module

[@view] let total_supply : (Token.t * storage) -> nat =
   FA2.Views.total_supply
```

### Extending parameters

The parameters can be also revisited and extended.

```ligolang
type parametric_parameter = FA2.parameter
type parametric_storage = Storage.t

type extension = Foo of nat

type parameter = extension parametric_parameter
type storage = (unit, Ledger.NFT.k, Ledger.NFT.v) parametric_storage

let extension (e:extension) (s:storage) (ledger:Ledger.NFT.t): operation list * storage = 
    (* do the job here ... *)
    [], s

let main : parameter * storage -> operation list * storage = 
   FA2.main Ledger.NFT.ledger_module extension

(*
   Views corner
*)

[@view] let balance_of : ((address * Token.t) * storage) -> nat =
   FA2.Views.balance_of Ledger.NFT.ledger_module

[@view] let total_supply : (Token.t * storage) -> nat =
   FA2.Views.total_supply
```

Finally you can extend the storate datatype and the parameter type.

## Internal Design

Contracts uses the same code but specific behaviors are given thanks to a record type (kind of module)

## LICENSE 

MIT License

Copyright (c) 2022 ligolang

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
