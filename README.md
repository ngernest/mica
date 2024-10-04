# Mica: Automated Differential Testing for OCaml Modules 

> **Note**: Mica is a research prototype and is not production-ready at the moment. Please contact Ernest Ng (`ernest@cs.cornell.edu`) if you'd like to contribute to Mica or have any questions!

**README Contents:**
- [Overview](#overview)
- [Compilation notes](#compilation-notes)
- [Case studies](#case-studies)
- [An overview of the codebase](#an-overview-of-the-codebase)
- [Differences between the OCaml Workshop '24 & ICFP '23 SRC artifacts](#differences-between-the-ocaml-workshop--icfp-src-artifacts)
- [Notes for implementors](#notes-for-implementors) 
    - [Testing changes locally](#testing-changes-locally)
    - [Dependencies](#dependencies)

## Overview
Mica is a PPX extension that automates differential testing for a pair of OCaml
modules implementing the same signature. Users annotate module signatures 
with the directive `[@@deriving mica]`, and at compile-time, Mica derives
specialized [property-based testing](https://www.youtube.com/watch?v=qmA9qhaECcE) (PBT) code that checks if two modules implementing the signature are observationally equivalent. (Under the hood, Mica uses Jane Street's [`Core.Quickcheck`](https://blog.janestreet.com/quickcheck-for-core/) PBT library.)

An online demo of Mica is available [**here**](https://ngernest.github.io/mica/demo.html).

Mica was presented at the OCaml Workshop '24 and the ICFP '23 SRC. The [OCaml Workshop paper](https://www.arxiv.org/abs/2408.14561) contains a lot more 
details about Mica's design -- this README focuses on describing how to interact with our OCaml artifact. 

Here is how we envisage users interacting with Mica:      
Suppose modules `M1` & `M2` both implement the module signature `S`. Users insert the directive `[@@deriving_inline mica]` beneath the definition of `S`, like so:
```ocaml
module type S = sig
  type 'a t 
  val empty : 'a t
  val add : 'a -> 'a t -> 'a t
  ...
end
[@@deriving_inline mica] 
...
[@@@end]
```
Then, after users run `dune build --auto-promote`, the derived PBT code is automatically inserted in-line in the source file in-between `[@@deriving_inline mica]` and `[@@@end]`. (Note: this doesn't work fully out of the box at the moment -- see [compilation notes](#compilation-notes) for details.)

Then, after running `dune build`, Mica derives the following PBT code:
```ocaml 
module Mica = struct 
  (** [expr] is an inductively-defined algebraic data type 
      representing {i symbolic expressions}. 
      
      Each [val] declaration in the module signature [S] corresponds to a 
      cosntructor for [expr] that shares the same name, arity & argument types. 
      - Type variables ['a] are instantiated with [int]
      - Function arguments of type ['a t] correpond to
        constructor arguments of type [expr] *)
  type expr =
    | Empty
    | Is_empty of expr
    ...
  [@@deriving show, ...]

  (** Types of symbolic expressions *)
  type ty = Int | IntT | ... [@@deriving show, ...]

  (** QuickCheck generator for symbolic expressions. 
      [gen_expr ty] generates random [expr]s of type [ty]. *)
  let rec gen_expr : ty -> Core.Quickcheck.Generator.t = ...

  (** Functor that interprets symbolic expressions *)
  module Interpret (M : S) = struct   
    (** Values of symbolic expressions *)
    type value = ValInt of int | ValIntT of int M.t | ...

    (** Big-step interpreter for symbolic expressions: 
        [interp] takes an [expr] and interprets it over the module 
        [M], evaluating the [expr] to a [value] *)
    let rec interp : expr -> value = ...
  end 

  (** Functor that tests [M1] and [M2] for observational equivalence *)
  module TestHarness (M1 : S) (M2 : S) = struct 
    (* Runs all observational equivalence tests *)
    let run_tests : unit -> unit = ... 
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
- **Note**: Mica only tests for observational equivalence at *concrete types* (e.g. `int`, `string option`), 
and not abstract types defined in a module (e.g. `'a M.t`), since abstract types have a more abstract notion 
of equality different from OCaml's standard notion of polymorphic equality. 

## Limitations
At the moment, Mica only works with module signatures that define one abstract type (e.g. `t` or `'a t`) and only contain pure functions. 
Modules with multiple abstract types and/or abstract types with multiple type parameters are not supported at the moment. 
 
## Compilation notes
There is a known issue with Ppxlib ([#338](https://github.com/ocaml-ppx/ppxlib/issues/338), [#342](https://github.com/ocaml-ppx/ppxlib/issues/342)) which causes Ppxlib to error when Dune is promoting changes (i.e. after one runs `dune build --auto-promote`, during which Dune inserts the code derived by Mica into the source file). 

To fix this issue, remove `[@@deriving_inline mica]` and `[@@@end]` from the source file while keeping the code inserted by Dune/Mica. Then, recompile by running `dune build again`. This second compilation run should complete successfully!

## Case Studies
Code for the following case studies (along with the code automatically derived by Mica) is located in the ancillary [`mica_case_studies`](https://github.com/ngernest/mica_case_studies) repo.      

We have tested Mica with the following module signatures, each of which is implemented by two different modules:

- Finite Sets (lists & BSTs) ([link](https://github.com/ngernest/mica_case_studies/tree/main/lib/sets))
- Regular Expression Matchers (Brzozowski Derivatives & DFAs) ([link](https://github.com/ngernest/mica_case_studies/tree/main/lib/regexes/))
- Polynomials (Horner schema & monomial-based representations) ([link](https://github.com/ngernest/mica_case_studies/tree/main/lib/polynomials/))
- Ephemeral Queues ([`Base.Queue`](https://ocaml.janestreet.com/ocaml-core/v0.13/doc/base/Base/Queue/index.html) & [`Base.Linked_queue`](https://ocaml.janestreet.com/ocaml-core/v0.12/doc/base/Base/Linked_queue/index.html)) ([link](https://github.com/ngernest/mica_case_studies/tree/main/lib/queues/))
- Unsigned integer arithmetic (the [`stdint`](https://github.com/andrenth/ocaml-stdint/tree/master) and [`ocaml-integers`](https://github.com/yallop/ocaml-integers) libraries) ([link](https://github.com/ngernest/mica_case_studies/tree/main/lib/unsigned_ints/))
- Character sets (the [`charset`](https://github.com/yallop/ocaml-charset) library & the standard library's `Set.Make(Char)` module) ([link](https://github.com/ngernest/mica_case_studies/tree/main/lib/charsets/))
- Persistent maps (red-black trees & association lists) ([link](https://github.com/ngernest/mica_case_studies/tree/main/lib/maps/))
- John Hughes's *How to Specify It* (catching bugs in BST implementations) ([link](https://github.com/ngernest/mica_case_studies/tree/main/lib/how_to_specify_it/))
- UPenn CIS 1200 student homework submissions ([link](./lib/student_submissions/))
  - Note: no student code is available for this case study (to avoid posting homework solutions online)

For more details regarding these case studies, we refer the reader to the [`mica_case_studies`](https://github.com/ngernest/mica_case_studies) repo as well as the [OCaml Workshop paper](https://www.arxiv.org/abs/2408.14561).


## An overview of the codebase
The [`lib`](./lib) subdirectory contains the code for the Mica PPX deriver.      
The PPX code is organized as follows:
- [`ppx_mica.ml`](./lib/ppx_mica.ml): Declares the main Mica PPX deriver
- [`type_deriver.ml`](./lib/type_deriver.ml): Derives the definitions of auxiliary data types & the `gen_expr` Quickcheck generator
- [`interp_deriver.ml`](./lib/interp_deriver.ml): Derives the `Interpret` functor
- [`test_harness_deriver.ml`](./lib/test_harness_deriver.ml): Derives the `TestHarness` functor
- [`overall_deriver.ml`](./lib/overall_deriver.ml): Produces a module called `Mica` containing all the automatically derived code
- [`utils.ml`](./lib/utils.ml): Includes all the following helper modules for convenience:
  - [`builders.ml`](./lib/builders.ml): Functions for creating AST nodes
  - [`getters.ml`](./lib/getters.ml): Functions for inspecting AST nodes
  - [`equality.ml`](./lib/equality.ml): Location-agnostic equality functions for `Parsetree` types 
  - [`lident.ml`](./lib/lident.ml): Utilities for working with the `Longident` type
  - [`names.ml`](./lib/names.ml): Functions for generating fresh variable names & quoting expressions
  - [`printers.ml`](./lib/printers.ml): Pretty-printers for AST types + functions for monomorphizing types
  - [`errors.ml`](./lib/errors.ml): Functions for error handling (embedding errors as extension nodes in the derived code)
  - [`inv_ctx.ml`](./lib/inv_ctx.ml): The "inverse typing context", mapping types `ty` to expressions of type `ty`
  - [`let_open.ml`](./lib/let_open.ml): Helpers for producing `let open` expressions
  - [`include.ml`](./lib/include.ml): Helpers for producing `include` statements
  - [`miscellany.ml`](./lib/miscellany.ml): Miscellaneous helpers for working with lists & strings

The ancillary [`mica_tyche_utils`](https://github.com/ngernest/mica_tyche_utils) repo contains a small library for 
creating JSON files that are ingested by [Tyche](https://github.com/tyche-pbt/).
- Note: the Tyche-Mica integration is still work in progress (contact [Ernest Ng](mailto:ernest@cs.cornell.edu) for more details).
- For more details about Tyche, we refer the reader to the [UIST '24 paper by Goldstein et al](https://harrisongoldste.in/papers/uist24-tyche.pdf). 

## Differences between the OCaml Workshop & ICFP SRC artifacts
- The OCaml Workshop artifact (2024) includes a version of Mica that has been  
  re-implemented from scratch as a PPX deriver (using `Ppxlib`). This artifact
  derives code at compile-time and is more feature rich (e.g. is compatible with Tyche). 
  - The `main` branch of this repo currently contains the OCaml Workshop artifact.

- The ICFP SRC artifact (2023) contained a Mica prototype that was implemented 
  as a command-line script. This prototype contained a parser for ML module signatures
  (written using `Angstrom`) and pretty-printed the derived PBT code to a new `.ml` file
  (using `PPrint`). This artifact derived code at runtime and is less robust compared to 
  the newer OCaml Workshop artifact.
  - Contact Ernest (`ernest@cs.cornell.edu`) for access to the (now-deprecated) ICFP '23 SRC artifact. 

## Notes for Implementors
### Testing changes locally
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
- This repo has been tested with OCaml 5.0.0 on an M1 Mac.
- To install all dependencies required for local development, run `make install`
- We recommend having the following libraries installed:
  - `ppxlib`
  - `ppx_jane`
  - `ppx_deriving.show`
  - `base`
  - `base_quickcheck`
  - `core`
  - `core_unix`
  - `alcotest`
- You can also run `dune utop` to see the output of functions in the codebase in a REPL.
