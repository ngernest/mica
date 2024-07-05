open Ppxlib

(******************************************************************************)
(** {1 Quoting names} *)

(** Turns the variable [x] into [x'] *)
val add_prime : string -> string

(** [update_expr_arg_names expr_args args] replaces each variable [x] in 
    [expr_args] if [x'] (the variable with a prime added) is in [expr_args] *)
val update_expr_arg_names : string list -> string list -> string list

(******************************************************************************)
(** {1 Producing fresh identifiers} *)

(** Produces a fresh identifier of type at location [loc], with the type [ty]
    of the variable serialized & prefixed to the resultant variable name 
    - Note that the type of the resultant identifier is polymorphic:
      The function argument [f] specifies how to take a [Location.t]
       and a [string] to form the desired identifier type. *)
val mk_fresh :
  loc:Location.t -> f:(loc:Location.t -> string -> 'a) -> core_type -> 'a

(** Makes a fresh identifier of type [pattern] (a [Ppat_var]) for type [ty]
  - [mk_fresh_pvar] is [mk_fresh], specialized to [pattern]s *)
val mk_fresh_pvar : loc:Location.t -> core_type -> pattern

(** Makes a fresh identifier of type [expression] (a [Pexp_ident]) for type [ty]
    - [mk_fresh_pvar] is [mk_fresh], specialized to [expression]s *)
val mk_fresh_evar : loc:Location.t -> core_type -> expression

(** Produces fresh identifiers for [args] at [loc] 
    - Like [mk_fresh], the resultant identifier type is polymorphic: 
      The function argument [f] specifies how to take a [Location.t]
      and a [string] to form the desired identifier type. *)
val varnames_of_cstr_args :
  loc:Location.t ->
  constructor_arguments ->
  f:(loc:Location.t -> string -> 'a) ->
  'a list

(** Takes [constructor_arguments] and produces a list of fresh identifiers 
    which each have type [pattern] 
    - This function is [varnames_of_cstr_args], specialized to [pattern]s *)
val pvars_of_cstr_args : loc:Location.t -> constructor_arguments -> pattern list

(** Takes [constructor_arguments] and produces a list of fresh identifiers 
    which each have type [expression] 
    - This function is [varnames_of_cstr_args], specialized to [expression]s *)
val evars_of_cstr_args :
  loc:Location.t -> constructor_arguments -> expression list

(******************************************************************************)
(** {1 Convering between different identifier types} *)

(** Takes a [constructor_declaration] and produces the pattern 
    [Ppat_construct] *)
val ppat_construct_of_cstr_decl :
  loc:Location.t -> constructor_declaration -> pattern
