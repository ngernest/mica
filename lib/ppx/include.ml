open Ppxlib
open Ast_builder.Default

(** Produces an [include] [structure_item] at location [loc] 
    for some [module_expr] [ME] *)
let include_module_expr ~(loc : Location.t) (mod_expr : module_expr) :
  structure_item =
  pstr_include ~loc (include_infos ~loc mod_expr)

(** Produces an [include] [structure_item] at location [loc] for 
    some [structure] *)
let include_structure ~(loc : Location.t) (structure : structure) :
  structure_item =
  include_module_expr ~loc (pmod_structure ~loc structure)
