val with_loc : 'a -> loc:'b -> 'c

val no_loc : 'b -> 'a

val map2 : f:('a -> 'b) -> 'a * 'a -> 'b * 'b

val tuple4_to_pair : 'a * 'b * 'c * 'd -> 'a * 'b

val list_is_empty : 'a list -> bool

val list_or : bool list -> bool

val remove_last : 'a list -> 'a list

val get_last : 'a list -> 'a

val abstract_ty_name : string

val lident_loc_of_string : string -> loc:'a -> 'b

val string_of_lident : 'a -> string

val add_lident_prefix : string -> 'a -> 'b

val add_lident_loc_prefix : string -> 'a -> 'b

val err_fmt : 'a

val base_types : loc:'a -> 'b list

val pexp_ident_of_string : string -> loc:'a -> 'b

val ppat_var_of_string : string -> loc:'a -> 'b

val mk_cstr : name:string -> loc:'a -> arg_tys:'b list -> 'c

val get_type_params : 'a -> 'b list

type inv_ctx = ('a * string) list

val empty_ctx : inv_ctx

val add_prime : string -> string

val get_varname : 'a -> 'b

val get_cstr_args : loc:'b -> ('a -> 'c) -> 'a list -> 'd * inv_ctx

val find_exprs : inv_ctx -> string list

val get_cstr_metadata : ('a * 'b) list -> ('c * 'd option * inv_ctx * 'e) list

val get_cstr_metadata_minimal : 'a list -> ('b * 'c option) list

val get_cstr_name : 'a -> 'b

val get_cstrs_of_ty_decl : 'a -> ('b * 'c option) list

val mk_adt : loc:'a -> name:string -> cstrs:'b list -> 'c

val mk_error : local:'a -> global:'b -> 'c -> 'd

val attr : loc:'a -> name:string -> 'b

val mk_valt_pat : ?abs_ty_parameterized:bool -> string -> loc:'a -> 'b

val get_match_arm : string list -> abs_ty_parameterized:bool -> loc:'a -> 'b

val get_unary_case_rhs : 'a -> string -> 'b -> string -> loc:'c -> 'd

val get_nary_case_rhs : 'a -> string -> 'b -> 'c list -> loc:'d -> 'e

val update_expr_arg_names : string list -> string list -> string list

val mk_scrutinees : string list -> post:('a list -> 'b) -> loc:'c -> 'b

val get_ty_name_and_params : 'a -> 'b

val get_ty_decls_from_sig : 'a -> (string * 'b list) list

