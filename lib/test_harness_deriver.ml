open Ppxlib
open Ast_builder.Default
open Utils

(** Derives the [TestHarness] functor *)
let generate_functor ~(ctxt : Expansion_context.Deriver.t)
  (mt_decl : module_type_declaration) : structure =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  match mt_decl with
  | { pmtd_type = Some _; pmtd_name; _ } ->
    let sig_name = pmty_ident ~loc (map_with_loc ~f:Longident.parse pmtd_name) in
    [%str
      module TestHarness (M1 : [%m sig_name]) (M2 : [%m sig_name]) = struct
        module I1 = Interpret (M1)
        module I2 = Interpret (M2)
      end]
  | { pmtd_type = None; _ } ->
    Location.raise_errorf ~loc
      "Can't derive for expressions that aren't module type declarations"
