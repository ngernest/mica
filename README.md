# ppx_mica (WIP)

Current progress:
We have two PPX derivers (`mica_types` & `mica`), which take a 
module signature of the form 
```ocaml
module type S = sig
  type 'a t 
  val empty : 'a t
  val is_empty : 'a t -> bool
  val mem : 'a -> 'a t -> bool
  ...
end
[@@deriving_inline mica_types, mica] 
  (* Auto-generated code is pasted inline into the source file here *)
[@@@end]
```
and derives the following type, function and functor definitions:
```ocaml 
(* Boilerplate omitted *)

(** Symbolic expressions *)
type expr =
  | Empty
  | Is_empty of expr
  ...
[@@deriving show { with_path = false }]

(** Types of symbolic expressions *)
type ty = Int | IntT | ...

(** QuickCheck generator for symbolic expressions of type [ty] *)
let rec gen_expr ty = ...

(** Functor that interprets symbolic expressions *)
module Interpret (M : S) = struct   
  (* Values of symbolic expressions *)
  type value = ValInt of int | ValIntT of int M.t | ...

  (* Interprets symbolic expressions over [M] *)
  let rec interp (e : expr) : value = ...
end 
```
The datatype definitions are produced by the `mica_types` PPX deriver 
which is executed first, and the functor definition is produced by 
the main `mica` deriver which runs afterwards. 

**Functionality to be implemented**:
- Automatically derive the following `TestHarness` functor:
```ocaml
module TestHarness (M1 : S) (M2 : S) = struct 
   (* Tests [M1] and [M2] for observational equivalence *)
end
```
- Add optimizations to `gen_expr` (e.g. when `size` = 0, return nullary constructors)
- Generate random `int -> int` functions in `gen_expr` 
- Automatically derive the `Seq` constructor for testing imperative code
- Automatically instrument testing code with boilerplate needed for Tyche integration

## Directory overview
- [ppx_mica.ml](./lib/ppx_mica.ml): Declares the PPX deriver
- [type_deriver.ml](./lib/type_deriver.ml): Derives type definitions + the `gen_expr` Quickcheck generator
- [functor_deriver.ml](./lib/functor_deriver.ml): Derives the `TestHarness` functor (including the `interp` function)
- [utils.ml](./lib/utils.ml): Includes all the following helper modules for convenience:
  - [builders.ml](./lib/builders.ml): Functions for creating AST nodes
    - Most functions in this file begin with the prefix `mk_` 
  - [getters.ml](./lib/getters.ml): Functions for inspecting AST nodes
    - Most functions in this file begin with the prefix `get_`
  - [equality.ml](./lib/equality.ml): Location-agnostic equality functions for `Parsetree` types 
  - [lident.ml](./lib/lident.ml): Utilities for working with the `Longident` type
  - [names.ml](./lib/names.ml): Functions for generating fresh variable names & quoting expressions
  - [printers.ml](./lib/printers.ml): Pretty-printers for AST types
  - [errors.ml](./lib/errors.ml): Functions for error handling (embedding errors as extension nodes in the derived code)
  - [inv_ctx.ml](./lib/inv_ctx.ml): The "inverse typing context", mapping types `ty` to expressions of type `ty`
  - [let_open.ml](./lib/let_open.ml): Helpers for producing `let open` expressions
  - [miscellany.ml](./lib/miscellany.ml): Miscellaneous helpers for working with lists & strings
- [json_utils.ml](./lib/json_utils.ml): Tyche-related JSON utilities for collecting test statistics


## Testing 
1. [`test/utils_test`](./test/utils_test/) contains `Alcotest` unit tests for various helper functinos.
- See the [README](./test/utils_test/README.md) in [`utils_test`](./test/utils_test/) for instructions
on how to add new tests to the Alcotest test suite.
2. `bisect_ppx` is used to compute test coverage.
- Run `make coverage` to generate a test coverage report.       
(Note: before doing so, make sure `test/ppx_test/errors/dune.inc` is blank.)
3. `test/ppx_test` contains `.ml` test files used to test the PPX functionality
- To add a new test file to the corpus of test files, in either `ppx_test/passing` 
or `ppx_test/errors`, run:
```bash
$ touch test_file.{ml,expected} && dune runtest --auto-promote
```
(This automatically generates new Dune stanzas that are needed for 
the new test files to compile.)
- Then fill in the newly-created `.ml` and `.expected` files with the 
module signature under test (plus relevant PPX annotations), 
and execute `dune runtest`. If the test output from Dune looks good, 
run `dune promote` to update the `.expected` file with the contents 
of the `.actual` file (which contains what the PPX actually generated from that test run). 

## Local Development
- Have the following installed:
  - `ppxlib`
  - `ppx_jane`
  - `ppx_deriving.show`
  - `core`
  - `alcotest`
  - `bisect_ppx`
- Note: in this codebase, we typically rely on the OCaml standard library, 
  but in a few (limited) instances, we use Jane Street's `Base` library. 
  To avoid namespace clashes, whenever we use a function from `Base`, 
  we always do a local `let open` within the body of the caller function.

