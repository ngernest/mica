open Ppxlib

(******************************************************************************)
(** {1 Working with [module_expr]s} *)

val module_expr_of_string : loc:Location.t -> string -> module_expr
val let_open : loc:Location.t -> module_expr -> expression -> expression

val let_open_twice :
  loc:Location.t -> module_expr -> module_expr -> expression -> expression

(******************************************************************************)
(** {1 Utility functions for working with Ppxlib} *)

val add_prime : string -> string
val is_abs_ty_parameterized : signature -> bool

val get_match_arm :
  string list -> abs_ty_parameterized:bool -> loc:Location.t -> pattern

val get_unary_case_rhs :
  Longident.t Location.loc ->
  string ->
  Longident.t Location.loc ->
  string ->
  loc:Location.t ->
  expression

val get_nary_case_rhs :
  constructor_declaration ->
  string ->
  Longident.t Location.loc ->
  expression list ->
  loc:Location.t ->
  expression

val update_expr_arg_names : string list -> string list -> string list

(* -------------------------------------------------------------------------- *)
(*                      Helpers for deriving monadic code                     *)
(* -------------------------------------------------------------------------- *)

val monadic_bindop : loc:Location.t -> string -> expression -> binding_op

val let_monadic_bind :
  loc:Location.t -> string -> expression -> expression -> expression
