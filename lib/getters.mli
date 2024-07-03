open Ppxlib
open Inv_ctx

(******************************************************************************)
(** {1 Working with constructors for algebraic data types} *)

val get_varname : pattern -> string

val get_cstr_arg_tys :
  ?is_arrow:bool -> core_type -> string list -> core_type list

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

val get_cstr_arity : constructor_declaration -> int

(******************************************************************************)
(** {1 Working with type parameters & type declarations} *)

val get_type_params : type_declaration -> core_type list
val get_ret_ty : core_type -> core_type
val get_ty_name_and_params : type_declaration -> string * core_type list
val get_ty_decls_from_sig : signature -> (string * core_type list) list
val get_abs_tys_from_sig : signature -> type_declaration list
val get_abs_ty_names : signature -> string list

(******************************************************************************)
(** {1 Working with pattern matches} *)

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
