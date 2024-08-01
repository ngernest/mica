open Ppxlib
open StdLabels

(******************************************************************************)
(* Deriver for types *)

(** Instantiates the PPX deriver for [expr]s *)
let type_generator =
  Deriving.Generator.V2.make_noarg Type_deriver.generate_types_from_sig

(** Registers the PPX deriver for the [expr] & [ty] type definitions *)
let type_deriver =
  Deriving.add "mica_types" ~str_module_type_decl:type_generator

(******************************************************************************)
(* Deriver for [Interpret] functor *)

let interp_functor_gen_wrapper ~ctxt mt =
  [ Interp_deriver.generate_functor ~ctxt mt ]

(** Instantiates the PPX deriver for the [Interpret] functor *)
let interp_functor_gen =
  Deriving.Generator.V2.make_noarg interp_functor_gen_wrapper
    ~deps:[ type_deriver ]

(** Registers the PPX deriver for the [Interpret] functor *)
let interp_deriver =
  Deriving.add "mica_interp_functor" ~str_module_type_decl:interp_functor_gen

(******************************************************************************)
(* Deriver for [TestHarness] functor *)

let test_harness_functor_gen =
  Deriving.Generator.V2.make_noarg Test_harness_deriver.generate_functor
    ~deps:[ interp_deriver ]

let test_harness_deriver =
  Deriving.add "mica_test_harness"
    ~str_module_type_decl:test_harness_functor_gen

(******************************************************************************)

let mica_module_gen =
  Deriving.Generator.V2.make_noarg Overall_deriver.generate_mica_module

let mica_module_deriver =
  Deriving.add "mica_overall" ~str_module_type_decl:mica_module_gen

(******************************************************************************)
(* Main [mica] deriver *)

(** Registers the main [mica] PPX deriver *)
let () =
  List.iter ~f:Reserved_namespaces.reserve
    [ "mica_types";
      "mica_interp_functor";
      "mica_test_harness";
      "mica_overall";
      "mica"
    ];

  let derivers = [ test_harness_deriver; interp_deriver; type_deriver ] in
  (* Add an alias so that users just need to write [[@@deriving mica]] *)
  Deriving.add_alias "mica" derivers |> Deriving.ignore
