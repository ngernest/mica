open Ppxlib

val pexp_ident_of_string : string -> loc:Location.t -> expression
val ppat_var_of_string : string -> loc:Location.t -> pattern
val mk_fresh_ppat_var : loc:Location.t -> core_type -> pattern

val varnames_of_cstr_args :
  loc:Location.t -> constructor_arguments -> pattern list

val ppat_construct_of_cstr_decl :
  loc:Location.t ->
  constructor_declaration ->
  pattern option ->
  pattern
