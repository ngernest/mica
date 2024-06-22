open Ppxlib

(******************************************************************************)
(** {1 Longident utility functions} *)

val uncapitalize : string -> string
val lident_loc_of_string : string -> loc:Location.t -> Longident.t Location.loc
val uncapitalize_lident : Longident.t -> Longident.t
val string_of_lident : Longident.t -> string
val add_lident_prefix : string -> Longident.t -> Longident.t

val add_lident_loc_prefix :
  string -> Longident.t Location.loc -> Longident.t Location.loc

(******************************************************************************)
(** {1 Working with [module_expr]s} *)

val module_expr_of_string : loc:Location.t -> string -> module_expr
val let_open : loc:Location.t -> module_expr -> expression -> expression

val let_open_twice :
  loc:Location.t -> module_expr -> module_expr -> expression -> expression

(******************************************************************************)
(** {1 Pretty-printers} *)

val pp_pattern : pattern -> unit
val pp_core_type : core_type -> unit
val pp_expression : expression -> unit
val pp_structure_item : structure_item -> unit
val string_of_core_ty : core_type -> string
val equal_core_type_ty_cstr : core_type -> constructor_declaration -> bool 

(******************************************************************************)
(** {1 Utility functions for working with Ppxlib} *)

val base_types : loc:Location.t -> core_type list
val pexp_ident_of_string : string -> loc:Location.t -> expression
val ppat_var_of_string : string -> loc:Location.t -> pattern

val mk_cstr :
  name:string ->
  loc:Location.t ->
  arg_tys:core_type list ->
  constructor_declaration

val monomorphize : core_type -> core_type
val get_type_params : type_declaration -> core_type list

type inv_ctx = (core_type * string) list

val empty_ctx : inv_ctx
val mk_fresh : loc:Location.t -> int -> core_type -> pattern
val add_prime : string -> string
val get_varname : pattern -> string
val get_cstr_arg_tys : ?is_arrow:bool -> core_type -> core_type list
val get_ret_ty : core_type -> core_type

val get_cstr_args :
  loc:Location.t -> ('a -> core_type) -> 'a list -> pattern * inv_ctx

val find_exprs : inv_ctx -> string list

val get_cstr_metadata :
  (constructor_declaration * core_type) list ->
  (Longident.t Location.loc * pattern option * inv_ctx * core_type) list

val get_cstr_metadata_minimal :
  constructor_declaration list ->
  (Longident.t Location.loc * pattern option) list

val get_cstr_name : constructor_declaration -> Longident.t Location.loc

val get_cstrs_of_ty_decl :
  type_declaration -> (Longident.t Location.loc * pattern option) list

val mk_adt :
  loc:Location.t ->
  name:string ->
  cstrs:constructor_declaration list ->
  type_declaration

val mk_error :
  local:Location.t ->
  global:Location.t ->
  (extension, Format.formatter, unit, extension) format4 ->
  structure_item

val attr : loc:Location.t -> name:string -> attribute
val is_abs_ty_parameterized : signature -> bool

val mk_valt_pat :
  ?abs_ty_parameterized:bool -> string -> loc:Location.t -> pattern

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

val mk_scrutinees :
  string list ->
  post:(expression list -> expression) ->
  loc:Location.t ->
  expression

val get_ty_name_and_params : type_declaration -> string * core_type list
val get_ty_decls_from_sig : signature -> (string * core_type list) list

(* -------------------------------------------------------------------------- *)
(*                      Helpers for deriving monadic code                     *)
(* -------------------------------------------------------------------------- *)

val monadic_bindop : loc:Location.t -> string -> expression -> binding_op

val let_monadic_bind :
  loc:Location.t -> string -> expression -> expression -> expression

(*******************************************************************************)
(** {1 Miscellany} *)

val printf : ('a, Stdio.Out_channel.t, unit) format -> 'a
val with_loc : 'a -> loc:Location.t -> 'a Location.loc
val no_loc : 'a Location.loc -> 'a
val map2 : f:('a -> 'b) -> 'a * 'a -> 'b * 'b
val tuple4_to_pair : 'a * 'b * 'c * 'd -> 'a * 'b
val list_is_empty : 'a list -> bool
val list_or : bool list -> bool
val remove_last : 'a list -> 'a list
val get_last : 'a list -> 'a
