# Mica: Automated Differential Testing for OCaml Modules 

(**Note**: Mica is a research prototype and is not production-ready at the moment.)

## Overview
Mica is a PPX extension that automates differential testing for a pair of OCaml
modules implementing the same signature. Users annotate module signatures 
with the directive `[@@deriving mica]`, and at compile-time, Mica derives
specialized property-based testing code (using Jane Street's `Core.Quickcheck` library)
that checks if two modules implementing the signature are observationally equivalent.

Here is how we envisage users interacting with Mica. Suppose multiple modules
implement the module signature `S`. Users insert the directive `[@@deriving_inline mica]`
beneath the definition of `S`, like so:
```ocaml
module type S = sig
  type 'a t 
  val empty : 'a t
  val add : 'a -> 'a t -> 'a t
  ...
end
[@@deriving_inline mica] 
(* After users run [dune build --auto-promote], the derived PBT code is 
   automatically pasted inline into the source file here *)
...
[@@@end]
```

Then, after running `dune build`, Mica derives the following PBT code:
```ocaml 
module Mica = struct 
  (** Symbolic expressions *)
  type expr =
    | Empty
    | Is_empty of expr
    ...
  [@@deriving show, ...]

  (** Types of symbolic expressions *)
  type ty = Int | IntT | ... [@@deriving show, ...]

  (** QuickCheck generator for symbolic expressions of type [ty] *)
  let rec gen_expr : ty -> Core.Quickcheck.Generator.t = ...

  (** Functor that interprets symbolic expressions *)
  module Interpret (M : S) = struct   
    (* Values of symbolic expressions *)
    type value = ValInt of int | ValIntT of int M.t | ...

    (* Interprets symbolic expressions over [M] *)
    let rec interp : expr -> value = ...
  end 

  (** Functor that tests [M1] and [M2] for observational equivalence *)
  module TestHarness (M1 : S) (M2 : S) = struct 
    ...
  end
end
```
- Now suppose modules `M1` and `M2` both implement `S`. To run Mica's testing code
  and check whether `M1` & `M2` are observationally equivalent with respect to `S`, 
  one can invoke the `run_tests : unit -> unit` function in Mica's `TestHarness` functor, like so:
```ocaml 
module T = Mica.TestHarness(M1)(M2)
let () = T.run_tests ()
```

## Case Studies
Code for the following case studies (along with the code automatically derived by Mica) can be found in the [`case-studies`](./case_studies/) subdirectory:
- John Hughes's *How to Specify It* (catching bugs in BST implementations) ([link](./case_studies/how_to_specify_it/))
- UPenn CIS 1200 student homework submissions ([link](./case_studies/student_submissions/))
- Finite Sets (lists & BSTs)  ([link](./case_studies/sets/))
- Regular Expression Matchers (Brzozowski Derivatives & DFAs) ([link](./case_studies/regexes/))
- Polynomials (Horner schema & monomial-based representations) ([link](./case_studies/polynomials/))
- Ephemeral Queues (`Base.Queue` & `Base.Linked_queue`) ([link](./case_studies/queues/))
- Unsigned integer arithmetic (the `stdint` and `ocaml-integer` libraries) ([link](./case_studies/unsigned_ints/))
- Character sets (the `charset` library & the standard library's `Set.Make(Char)` module) 
- Persistent maps (red-black trees & association lists) ([link](./case_studies/maps/))

## An overview of the codebase
The [`lib/ppx`](./lib/ppx) subdirectory contains the code for the Mica PPX deriver.      
The PPX code is organized as follows:
- [`ppx_mica.ml`](./lib/ppx/ppx_mica.ml): Declares the main Mica PPX deriver
- [`type_deriver.ml`](./lib/ppx/type_deriver.ml): Derives the definitions of auxiliary data types & the `gen_expr` Quickcheck generator
- [`interp_deriver.ml`](./lib/ppx/interp_deriver.ml): Derives the `Interpret` functor
- [`test_harness_deriver.ml`](./lib/ppx/test_harness_deriver.ml): Derives the `TestHarness` functor
- [`overall_deriver.ml`](./lib/ppx/overall_deriver.ml): Produces a module called `Mica` containing all the automatically derived code
- [`utils.ml`](./lib/ppx/utils.ml): Includes all the following helper modules for convenience:
  - [`builders.ml`](./lib/ppx/builders.ml): Functions for creating AST nodes
  - [`getters.ml`](./lib/ppx/getters.ml): Functions for inspecting AST nodes
  - [`equality.ml`](./lib/ppx/equality.ml): Location-agnostic equality functions for `Parsetree` types 
  - [`lident.ml`](./lib/ppx/lident.ml): Utilities for working with the `Longident` type
  - [`names.ml`](./lib/ppx/names.ml): Functions for generating fresh variable names & quoting expressions
  - [`printers.ml`](./lib/ppx/printers.ml): Pretty-printers for AST types + functions for monomorphizing types
  - [`errors.ml`](./lib/ppx/errors.ml): Functions for error handling (embedding errors as extension nodes in the derived code)
  - [`inv_ctx.ml`](./lib/ppx/inv_ctx.ml): The "inverse typing context", mapping types `ty` to expressions of type `ty`
  - [`let_open.ml`](./lib/ppx/let_open.ml): Helpers for producing `let open` expressions
  - [`include.ml`](./lib/ppx/include.ml): Helpers for producing `include` statements
  - [`miscellany.ml`](./lib/ppx/miscellany.ml): Miscellaneous helpers for working with lists & strings

Additionally, the [`lib/tyche_utils`](./lib/tyche_utils) subdirectory contains a small library for 
creating JSON files that are ingested by Tyche (see [`tyche_utils.ml`](./lib/tyche_utils/tyche_utils.ml)). 

## Differences between the OCaml Workshop & ICFP SRC artifacts
- The OCaml Workshop artifact (2024) includes a version of Mica that has been  
  re-implemented from scratch as a PPX deriver (using `Ppxlib`). This artifact
  derives code at compile-time and is more feature rich (e.g. is compatible with Tyche). 
  - The `main` branch of this repo currently contains the OCaml Workshop artifact.

- The ICFP SRC artifact (2023) contains a Mica prototype that was implemented 
  as a command-line script. This prototype contains a parser for ML module signatures
  (written using `Angstrom`) and pretty-prints the derived PBT code to a new `.ml` file
  (using `PPrint`). This artifact derives code at runtime and is less robust compared to 
  the OCaml Workshop artifact.
  - [Link to SRC prototype artifact](https://github.com/ngernest/mica/releases/tag/icfp23src_artifact)).

## Notes for Implementors
### Testing 
1. [`test/utils_test`](./test/utils_test/) contains `Alcotest` unit tests for various helper functions.
- See the [README](./test/utils_test/README.md) in [`utils_test`](./test/utils_test/) for instructions
on how to add new tests to the Alcotest test suite.
2. `test/ppx_test` contains `.ml` test files used to test the PPX functionality
- To add a new test file to the corpus of test files, in either `ppx_test/passing` 
or `ppx_test/errors`, run:
```bash
$ touch test_file.{ml,expected}; dune runtest --auto-promote
```
(This automatically generates new Dune stanzas that are needed for 
the new test files to compile.)
- Then fill in the newly-created `.ml` and `.expected` files with the 
module signature under test (plus relevant PPX annotations), 
and execute `dune runtest`. If the test output from Dune looks good, 
run `dune promote` to update the `.expected` file with the contents 
of the `.actual` file (which contains what the PPX actually generated from that test run). 

### Dependencies
- This repo has been tested with OCaml 5.1.0 on an M1 Mac.
- We recommend having the following libraries installed:
  - `ppxlib`
  - `ppx_jane`
  - `ppx_deriving.show`
  - `base`
  - `base_quickcheck`
  - `core`
  - `core_unix`
  - `alcotest`
  - `yojson`
- The [`case-studies`](./case_studies/) subdirectory also requires the following libraries:
  - `charset`
  - `ocaml-integers`
  - `stdint`
- You can also run `dune utop` to see the output of functions in the codebase in a REPL.
