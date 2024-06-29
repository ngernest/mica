open Ppxlib

val monadic_bindop : loc:Location.t -> string -> expression -> binding_op

val let_monadic_bind :
  loc:Location.t -> string -> expression -> expression -> expression
