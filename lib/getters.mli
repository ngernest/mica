open Ppxlib
open Inv_ctx

(******************************************************************************)
(** {1 Working with constructors for algebraic data types} *)

(** Extracts the variable name from a [Ppat_var] pattern 
  - Raises [Not_found] if the input pattern is not of the form [Ppat_var] *)
val get_varname : pattern -> string

  (** Takes [ty], the type of a [val] declaration in a signature,
    and returns the type of the arguments of the corresponding 
    constructor for the [expr] datatype. 
    - The [abs_tys] argument is a list of abstract types
      that are defined in the module signature (to determine
      when arguments should be instantiated with the [expr] type)

    For the [Set] module signature example,
    - [val empty : 'a t] corresponds to the 0-arity [Empty] constructor
    - [val is_empty : 'a t -> bool] corresponds to [Is_empty of expr * bool] 
    - Monomorphic primitive types are preserved. 

    The [is_arrow] optional 
    named argument specifies whether [ty] is an arrow type: if yes, then 
    references to abstract types should be replaced with [expr], otherwise
    an occurrence of an abstract type in an non-arrow type 
    (e.g. [val empty : 'a t]) should be ignored (so [val empty : 'a t] 
    corresponds to the nullary constructor [Empty]). *)
val get_cstr_arg_tys :
  ?is_arrow:bool -> core_type -> string list -> core_type list

(** Helper function: [get_cstr_args loc get_ty args] takes [args], 
  a list containing the {i representation} of constructor arguments, 
  applies the function [get_ty] to each element of [args] and produces 
  a formatted tuple of constructor arguments (using the [ppat_tuple] smart 
  constructor for the [pattern] type).  
  - Note that [args] has type ['a list], i.e. the representation of 
  constructor arguments is polymorphic -- this function is instantiated 
  with different types when called in [get_cstr_metadata] *)
val get_cstr_args :
  loc:Location.t -> ('a -> core_type) -> 'a list -> pattern * inv_ctx

(** Takes a list of [constructor_declaration]'s and returns 
    a list consisting of 4-tuples of the form 
    (constructor name, constructor arguments, typing context, return type) *)  
val get_cstr_metadata :
  (constructor_declaration * core_type) list ->
  (Longident.t Location.loc * pattern option * inv_ctx * core_type) list

(** Variant of [get_cstr_metadata] which returns 
      only a list of pairs containing constructor names & constructor args *)  
val get_cstr_metadata_minimal :
  constructor_declaration list ->
  (Longident.t Location.loc * pattern option) list

(** Extracts the constructor name (along with its location) from 
    a constructor declaration *)
val get_cstr_name : constructor_declaration -> Longident.t Location.loc

(** Takes a [type_declaration] for an algebraic data type 
    and returns a list of (constructor name, constructor arguments) 
    - Raises an exception if the [type_declaration] doesn't correspond to an 
      algebraic data type *)
val get_cstrs_of_ty_decl :
  type_declaration -> (Longident.t Location.loc * pattern option) list

(** Computes the arity of a constructor for an algebraic data type *)  
val get_cstr_arity : constructor_declaration -> int

(******************************************************************************)
(** {1 Working with type parameters & type declarations} *)

(** [get_type_varams td] extracts the type parameters 
    from the type declaration [td]
    - Type variables (e.g. ['a]) are instantiated with [int] *)
val get_type_params : type_declaration -> core_type list

(** Extracts the (monomorphized) return type of a type expression 
    (i.e. the rightmost type in an arrow type) *)
val get_ret_ty : core_type -> core_type

(** Takes a [type_declaration] and returns a pair of the form 
    [(<type_name, list_of_type_parameters)] *)
val get_ty_name_and_params : type_declaration -> string * core_type list

(** Takes a module signature and returns a list containing pairs of the form
    [(<type_name>, <list_of_type_parameters>)]. The list is ordered based on
    the order of appearance of the type declarations in the signature.  *)
val get_ty_decls_from_sig : signature -> (string * core_type list) list

(** Retrieves all the abstract types from a signature as a list of 
    [type_declaration]s *)
val get_abs_tys_from_sig : signature -> type_declaration list

(** Retrieves the names of all the abstract types in a signature *)
val get_abs_ty_names : signature -> string list

(******************************************************************************)
(** {1 Working with pattern matches} *)

(** [get_match_arm ~loc expr_vars ~abs_ty_parameterized] returns the 
    match arms of the inner pattern match in [interp], e.g. 
    an expression of the form [ValIntT e]
    - The argument [expr_vars] is a list of variable names that 
    have type [expr]
    - The named argument [abs_ty_parameterized] represents whether the 
    abstract type [t] in the module signature is parameterized (e.g. ['a t]) *)
val get_match_arm :
  string list -> abs_ty_parameterized:bool -> loc:Location.t -> pattern

(** Creates the RHS of the inner pattern-match in [interp], for the special 
    case where we are dealing with a unary [value] constructor
    and a unary module function, e.g. [match e with ValInt x -> M.f x] 
    (In this example, [get_unary_case_rhs] produces the expression [M.f x])
    - [value_cstr] is the name of the constructor for the [value] type 
    - [expr_cstr] is the constructor for the [expr] type, which corresponds
    to a function inside the module with name [mod_name] 
    - [x] is the argument that will be applied to the module function *)  
val get_unary_case_rhs :
  Longident.t Location.loc ->
  string ->
  Longident.t Location.loc ->
  string ->
  loc:Location.t ->
  expression

(** Variant of [get_unary_case_rhs] which handles the situation 
    when the RHS of the case statement is an n-ary function with 
    arguments [xs] *)
val get_nary_case_rhs :
  constructor_declaration ->
  string ->
  Longident.t Location.loc ->
  expression list ->
  loc:Location.t ->
  expression
