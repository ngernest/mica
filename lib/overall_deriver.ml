open Ppxlib
open Ast_builder.Default

(** Produces an [include ME] [structure_item] at location [loc] 
    for some [module_expr] [ME] *)
let include_mod ~(loc : Location.t) (mod_expr : module_expr) : structure_item =
  pstr_include ~loc (include_infos ~loc mod_expr)

(** Produces a module called [Mica] that contains all the automatically derived 
    code. Note: this code derived by this function is the union of code derived 
    by [Type_deriver], [Interp_deriver] and [Test_harness_deriver]. *)
let generate_mica_module ~ctxt (mt : module_type_declaration) : structure =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  let type_defns : structure_item =
    include_mod ~loc
      (pmod_structure ~loc @@ Type_deriver.generate_types_from_sig ~ctxt mt)
  in
  let interp_functor : structure_item =
    Interp_deriver.generate_functor ~ctxt mt in
  let test_harness_functor : structure_item =
    include_mod ~loc
      (pmod_structure ~loc @@ Test_harness_deriver.generate_functor ~ctxt mt)
  in
  [%str
    module Mica = struct
      [%%i type_defns]
      [%%i interp_functor]
      [%%i test_harness_functor]
    end]
