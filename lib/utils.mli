open Ppxlib

(******************************************************************************)
(** {1 Utility functions for working with Ppxlib} *)

val is_abs_ty_parameterized : signature -> bool
val update_expr_arg_names : string list -> string list -> string list

(* -------------------------------------------------------------------------- *)
(*                      Helpers for deriving monadic code                     *)
(* -------------------------------------------------------------------------- *)

val monadic_bindop : loc:Location.t -> string -> expression -> binding_op

val let_monadic_bind :
  loc:Location.t -> string -> expression -> expression -> expression
