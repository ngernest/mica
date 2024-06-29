open Ppxlib
open Ast_helper
open Ast_builder.Default
open Miscellany

(** Constructs a [module_expr] from a string containing the module name *)
let module_expr_of_string ~(loc : Location.t) (str : string) : module_expr =
  pmod_ident ~loc (with_loc ~loc (Longident.parse str))

(** [let_open ~loc M e] creates the expression 
    [let open M in e], where [m] is some [module_expr] *)
let let_open ~(loc : Location.t) (m : module_expr) (e : expression) : expression
    =
  let mod_infos = open_infos ~loc ~expr:m ~override:Fresh in
  pexp_open ~loc mod_infos e

(** [let_open_twice ~loc M1 M2 e] produces the [expression] 
    [let open M1 in let open M2 in e] at location [loc],
    for [module_expr]s [M1] and [M2] *)
let let_open_twice ~(loc : Location.t) (m1 : module_expr) (m2 : module_expr)
  (e : expression) =
  let_open ~loc m1 (let_open ~loc m2 e)
