open Ppxlib
open Ast_builder.Default

let include_mod ~(loc : Location.t) (mod_expr : module_expr) : structure_item =
  pstr_include ~loc (include_infos ~loc mod_expr)

let generate_mica_module ~ctxt (mt : module_type_declaration) =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  let type_defns : module_expr =
    pmod_structure ~loc (Type_deriver.generate_types_from_sig ~ctxt mt) in
  let interp_functor : module_expr =
    pmod_structure ~loc (Interp_deriver.generate_functor ~ctxt mt) in
  [%str
    module Mica = struct
      [%%i include_mod ~loc type_defns]
      [%%i include_mod ~loc interp_functor]
    end]
