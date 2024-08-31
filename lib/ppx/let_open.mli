open Ppxlib

(** Constructs a [module_expr] from a string containing the module name *)
val module_expr_of_string : loc:Location.t -> string -> module_expr

(** [let_open ~loc M e] creates the expression 
    [let open M in e], where [m] is some [module_expr] *)
val let_open : loc:Location.t -> module_expr -> expression -> expression

(** [let_open_twice ~loc M1 M2 e] produces the [expression] 
    [let open M1 in let open M2 in e] at location [loc],
    for [module_expr]s [M1] and [M2] *)
val let_open_twice :
  loc:Location.t -> module_expr -> module_expr -> expression -> expression
