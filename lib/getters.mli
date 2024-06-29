open Ppxlib
open Inv_ctx

val get_type_params : type_declaration -> core_type list
val get_varname : pattern -> string
val get_ret_ty : core_type -> core_type
val get_cstr_arg_tys : ?is_arrow:bool -> core_type -> core_type list

val get_cstr_args :
  loc:Location.t -> ('a -> core_type) -> 'a list -> pattern * inv_ctx

val get_cstr_metadata :
  (constructor_declaration * core_type) list ->
  (Longident.t Location.loc * pattern option * inv_ctx * core_type) list

val get_cstr_metadata_minimal :
  constructor_declaration list ->
  (Longident.t Location.loc * pattern option) list

val get_cstr_name : constructor_declaration -> Longident.t Location.loc

val get_cstrs_of_ty_decl :
  type_declaration -> (Longident.t Location.loc * pattern option) list
