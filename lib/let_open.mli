open Ppxlib

val module_expr_of_string : loc:Location.t -> string -> module_expr
val let_open : loc:Location.t -> module_expr -> expression -> expression

val let_open_twice :
  loc:Location.t -> module_expr -> module_expr -> expression -> expression
