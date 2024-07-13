# ppx_mica (WIP)

Current progress:
We have two PPX derivers that take an module signature declaration of the form 
(in `bin/main.ml`):
```ocaml
(* bin/main.ml *)
module type SetInterface = sig
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
and produces the following type and functor definitions respectively:
```ocaml 
(* Boilerplate omitted *)

(** Symbolic expressions, monomorphized *)
type expr =
  | Empty
  | Is_empty of expr
  | Mem of int * expr
  ...

(** Types for symbolic expressions *)
type ty = Int | Bool | IntT 

(** Functor containing interpreter for symbolic expressions *)
module TestHarness(M : SetInterface) = struct 
  include M 
  
  type value = 
    | ValBool of bool 
    | ValInt of int 
    | ValIntT of int t

  (* Currently working on generating the RHS of the pattern-matches for 
     constructors with arguments *)
  let rec interp e =
    match e with
    | Empty -> ValIntT M.empty
    | Is_empty e1 ->
        (match interp e1 with
         | ValIntT e1' -> ValBool (M.is_empty e1')
         | _ -> failwith "impossible")
    | Mem (x1, e2) ->
        (match interp e2 with
         | ValIntT e2' -> ValBool (M.mem x1 e2')
         | _ -> failwith "impossible")
    | Union (e1, e2) ->
        (match (interp e1, interp e2) with
         | (ValIntT e1', ValIntT e2') -> ValIntT (M.union e1' e2')
         | _ -> failwith "impossible")
    ...
end 
```
The datatype definitions are produced by the `mica_types` PPX deriver 
which is executed first, and the functor definition is produced by 
the main `mica` deriver which runs afterwards. 

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
  - [bindop.ml](./lib/bindop.ml): Helpers for producing monadic bind expressions
  - [miscellany.ml](./lib/miscellany.ml): Miscellaneous helpers for working with lists & strings


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
  - `core`
  - `ppx_quick_test`
  - `alcotest`
  - `bisect_ppx`
- I recommend setting up an Opam switch for OCaml 5.1.0 and developing using 
  that compiler version (`ppx_quick_test` only works with OCaml 5.1 or newer). 
- Note: in this codebase, we typically rely on the OCaml standard library, 
  but in a few (limited) instances, we use Jane Street's `Base` library. 
  To avoid namespace clashes, whenever we use a function from `Base`, 
  we always do a local `let open` within the body of the caller function.

