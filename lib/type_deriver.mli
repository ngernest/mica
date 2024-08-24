open Ppxlib
open Inv_ctx

(** Walks over all the [val ...] declarations in a module signature
    and creates the corresponding definition of the [expr] ADT 
    - The return type is a list of pairs, where each pair 
    consists of the declaration for the [expr] constructor, 
    along with the return type of the function (expressed as 
    a [core_type]) *)
val mk_expr_cstrs : signature -> (constructor_declaration * core_type) list

(** Extracts the unique return types of all [val] declarations within a 
        module signature *)
val uniq_ret_tys : signature -> core_type list

(** Helper function for creating the constructors of the [ty] and [value] 
    algebraic data types 
    - The argument [sig_items] contains the contents of a module signature
    - [~f] is a function that specifies how to turn a [core_type] into a 
    [constructor_declaration] *)
val mk_cstr_aux :
  signature ->
  f:(core_type -> constructor_declaration) ->
  constructor_declaration list

(** Constructs the definition of the [ty] algebraic data type
    based on the unique return types of all [val] declarations within 
    the module signature *)
val mk_ty_cstrs : signature -> constructor_declaration list

(** [mk_val_cstr ty] constructors the corresponding constructor declaration
    for the [value] datatype, given some [core_type] [ty]
    - e.g. if [ty = Int], [mk_val_cstr] returns the declaration for 
      the [ValInt] constructor *)
val mk_val_cstr : core_type -> constructor_declaration

(** Constructs the definition of the [value] algebraic data type
    based on the inhabitants of the [ty] ADT *)
val mk_val_cstrs : signature -> constructor_declaration list

(** Takes the name of a type and produces the name of its 
    corresponding QuickCheck generator *)
val mk_generator_name : string -> string

(** Produces an atomic QuickCheck generator for the given [core_type]
    - [abs_tys] is an association list consisting of type names & type 
    parameters for the abstract types in the signature  *)
val gen_atom :
  loc:Location.t ->
  core_type ->
  abs_tys:(string * core_type list) list ->
  expression

(** Produces the name of QuickCheck generators corresponding to a list of 
    [constructor_declaration]s (by prepending the prefix "gen" to each 
    constructor's name) *)
val mint_generator_names : constructor_declaration list -> string list

(** Helper function for producing the RHS of the pattern match in gen_expr 
    - [abs_tys] is an association list consisting of type names & type 
    parameters for the abstract types in the signature *)
val gen_expr_rhs :
  loc:Location.t ->
  constructor_declaration list ->
  abs_tys:(string * core_type list) list ->
  expression

(** Determines if an [expr] constructor can be used as a base case for 
    [gen_expr]. A constructor can be used as the base case if:
    - It is nullary
    - It has no arguments of type [expr] (i.e. the corresponding function
      in the signature has no arguments of type [t]) 
    - Note: constructors with record arguments are currently unsupported. *)
val is_base_case : constructor_declaration -> bool

(** [check_type_is_concrete abs_ty_names ty] determines whether [ty] is a concrete 
    type based on [abs_ty_names], a list containing the names of abstract types 
    in a signature. 
    
    For example, if a module signature defines an abstract type ['a t], 
    then [int t] would {i not} be concrete, but [int] and [bool] would be 
    considered concrete. 
    - Note: type variables (e.g. ['a]) are considered concrete by this function
    (since they're technically not defined inside a module signature) *)
val check_type_is_concrete : string list -> core_type -> bool


(** [mk_gen_expr_case abs_tys ty rhs_cstrs] constructs a single case in the 
    pattern-match of the body of [gen_expr].
    - [abs_tys] is a list containing pairs of the form
    [(<type_name>, <list_of_type_parameters>)]. Most likely, this list is 
    obtained by calling [get_ty_decls_from_sig] in [getters.ml]. 
    - [ty] is the type we are matching on in the LHS of the pattern match 
    inside [gen_expr]
    - [rhs_cstrs] are the constructors for [expr] that have that type (to be 
    generated on the RHS of the pattern match).
    - [is_base_case] is an optional Boolean argument that indicates whether 
    the constructors in [rhs_cstrs] are base cases for [gen_expr] 
    (as determined by the [is_base_case] function). This parameter 
    defaults to [false]. *)
val mk_gen_expr_case :
  (string * core_type list) list ->
  ?is_base_case:bool ->
  core_type ->
  constructor_declaration list ->
  case

(** Creates the main case statement in [gen_expr] *)
val gen_expr_cases : signature -> case list

(** Derives the [gen_expr] QuickCheck generator 
    - [ty_cstrs] is a list of constructors for the [ty] ADT  *)
val derive_gen_expr : loc:Location.t -> signature -> expression

(** Produces the attribute [[@@deriving show { with_path = false }]] *)
val deriving_show : loc:Location.t -> attribute

(** Walks over a module signature definition and extracts the 
    abstract type declaration, producing the definition 
    the [expr] and [ty] algebraic data types *)
val generate_types_from_sig :
  ctxt:Expansion_context.Deriver.t ->
  module_type_declaration ->
  structure_item list

(** Helper function: given [mod_ty], a module signature,
    [get_expr_cstrs] produces [expr] constructor names & arguments
    that match the declarations in the module signature *)
val get_expr_cstrs :
  module_type ->
  (Longident.t Location.loc * pattern option * inv_ctx * core_type) list
