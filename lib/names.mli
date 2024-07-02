open Ppxlib

val pexp_ident_of_string : string -> loc:Location.t -> expression
val ppat_var_of_string : string -> loc:Location.t -> pattern
val add_prime : string -> string
val quote_name : string -> string
val mk_fresh_ppat_var : loc:Location.t -> core_type -> pattern

val varnames_of_cstr_args :
  loc:Location.t -> constructor_arguments -> pattern list

val ppat_construct_of_cstr_decl :
  loc:Location.t -> constructor_declaration -> pattern

val update_expr_arg_names : string list -> string list -> string list
