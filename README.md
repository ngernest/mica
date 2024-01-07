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
module ExprToImpl(M : SetInterface) = struct ... end 
```
The datatype definitions are produced by the `mica_types` PPX deriver 
which is executed first, and the functor definition is produced by 
the main `mica` deriver which runs afterwards. 

See `lib/lib.ml` for implementation details. 

## Testing 
1. `test/utils_test` contains `Alcotest` unit tests which test various helper functions
2. `test/ppx_test` contains `.ml` test files used to test the PPX functionality
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


