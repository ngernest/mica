open Ppxlib
open Ast_builder.Default
open Miscellany

(******************************************************************************)
(** {1 Helpers for deriving code using monadic bind (currently unused)} *)

let monadic_bindop ~(loc : Location.t) (x : string) (exp : expression) :
  binding_op =
  let op = with_loc ~loc "bind" in
  let pat = ppat_var ~loc (with_loc ~loc x) in
  binding_op ~loc ~op ~pat ~exp

let let_monadic_bind ~(loc : Location.t) (x : string) (e1 : expression)
  (e2 : expression) : expression =
  let letop = letop ~let_:(monadic_bindop ~loc x e1) ~ands:[] ~body:e2 in
  pexp_letop ~loc letop
