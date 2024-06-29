open Ppxlib
open StdLabels
open Lib

(** Instantiates the PPX deriver for [expr]s *)
let type_generator :
  (structure_item list, module_type_declaration) Deriving.Generator.t =
  Deriving.Generator.V2.make_noarg generate_types_from_sig

(** Labelled arguments for the [mica] PPX deriver 
    TODO: handle the continuation in [generate_functor] *)
let args () =
  Deriving.Args.(empty +> arg "m1" (pexp_ident __) +> arg "m2" (pexp_ident __))

(** Registers the main [mica_types] and [mica] PPX derivers *)  
let () =
  List.iter ~f:Reserved_namespaces.reserve [ "mica_types"; "mica" ];
  (* Generate auxiliary type declarations *)
  let datatype_deriver =
    Deriving.add "mica_types" ~str_module_type_decl:type_generator in
  (* Generate the body of the [TestHarness] functor - Note that we must generate
     the declarations of auxiliary datatypes before generating the functor *)
  let functor_generator =
    Deriving.Generator.V2.make_noarg ~deps:[ datatype_deriver ] generate_functor
  in
  Deriving.add "mica" ~str_module_type_decl:functor_generator |> Deriving.ignore
