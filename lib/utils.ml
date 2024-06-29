open Ppxlib
open Ast_helper
open Ast_builder.Default
open StdLabels
open Equality
open Lident
open Miscellany
open Printers
open Getters
open Builders

(******************************************************************************)
(** {1 Working with [module_expr]s} *)

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

(******************************************************************************)
(** {1 Utility functions for working with Ppxlib} *)

(******************************************************************************)

(** Returns true the abstract type declaration in a [signature] 
    is parameterized (e.g. ['a t]), else returns [false] *)
let rec is_abs_ty_parameterized (sig_items : signature) : bool =
  List.fold_left
    ~f:(fun acc {psig_desc; _} ->
      match psig_desc with
      | Psig_type (_rec_flag, ty_decls) -> (
        match ty_decls with
        | [] ->
            acc
        | _ ->
            list_or
            @@ List.map
                 ~f:(fun {ptype_name; ptype_params; _} ->
                   String.equal ptype_name.txt "t"
                   && not (list_is_empty ptype_params) )
                 ty_decls )
      | _ ->
          acc )
    ~init:false sig_items

(** [update_expr_arg_names expr_args args] replaces each variable [x] in 
    [expr_args] if [x'] (the variable with a prime added) is in [expr_args] *)
let update_expr_arg_names (expr_args : string list) (args : string list) :
    string list =
  List.map args ~f:(fun x ->
      if List.mem (add_prime x) ~set:expr_args then add_prime x else x )

(* -------------------------------------------------------------------------- *)
(*      Helpers for deriving monadic code (currently unused)                  *)
(* -------------------------------------------------------------------------- *)

let monadic_bindop ~(loc : Location.t) (x : string) (exp : expression) :
    binding_op =
  let op = with_loc ~loc "bind" in
  let pat = ppat_var ~loc (with_loc ~loc x) in
  binding_op ~loc ~op ~pat ~exp

let let_monadic_bind ~(loc : Location.t) (x : string) (e1 : expression)
    (e2 : expression) : expression =
  let letop = letop ~let_:(monadic_bindop ~loc x e1) ~ands:[] ~body:e2 in
  pexp_letop ~loc letop
