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

(** Instantiates the PPX deriver for the [Interpret] functor *)
let interp_functor_gen =
  Deriving.Generator.V2.make_noarg Functor_deriver.generate_functor

(** Registers the PPX deriver for the [Interpret] functor *)
let interp_functor_deriver =
  Deriving.add "mica_functor" ~str_module_type_decl:interp_functor_gen

(******************************************************************************)
(* Main [mica] deriver *)

(** Registers the main [mica] PPX deriver *)
let () =
  List.iter ~f:Reserved_namespaces.reserve
    [ "mica_types"; "mica_functor"; "mica" ];
  (* Add an alias so that users just need to write [[@@deriving mica]] *)
  Deriving.add_alias "mica" [ interp_functor_deriver; type_deriver ]
  |> Deriving.ignore
