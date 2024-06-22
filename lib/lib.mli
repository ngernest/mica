open Ppxlib
open Utils

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

(** Derives the [gen_expr] QuickCheck generator 
    - [ty_cstrs] is a list of constructors for the [ty] ADT  *)
val derive_gen_expr :
  loc:Location.t -> constructor_declaration list -> expression

(** Walks over a module signature definition and extracts the 
    abstract type declaration, producing the definition 
    the [expr] and [ty] algebraic data types *)
val generate_types_from_sig :
  ctxt:Expansion_context.Deriver.t ->
  module_type_declaration ->
  structure_item list

(** Instantiates the PPX deriver for [expr]s *)
val type_generator :
  (structure_item list, module_type_declaration) Deriving.Generator.t

(** Helper function: given [mod_ty], a module signature,
    [get_expr_cstrs] produces [expr] constructor names & arguments
    that match the declarations in the module signature *)
val get_expr_cstrs :
  module_type ->
  (Longident.t Location.loc * pattern option * inv_ctx * core_type) list

type interp_case_rhs_params =
  { loc : Location.t;
    mod_name : string;
    abs_ty_parameterized : bool;
    expr_cstr : Longident.t Location.loc;
    args : pattern option;
    gamma : inv_ctx;
    ret_ty : core_type
  }

(** Creates the body of the inner case-statement inside [interp]
  - NB: [gamma] is the "inverse typing context" which maps types 
    to variable names *)
val mk_interp_case_rhs : interp_case_rhs_params -> expression

(** Creates the definition for the [interp] function 
    (contained inside the body of the [ExprToImpl] functor) 
    - The argument [expr_cstrs] is a list containing the 
    names & arg types of the constructors for the [expr] algebraic data type *)
val mk_interp :
  loc:location ->
  module_type ->
  ?abs_ty_parameterized:bool ->
  (Longident.t Location.loc * pattern option * inv_ctx * core_type) list ->
  structure_item

(** Creates the body of the [ExprToImpl] functor *)
val mk_functor :
  loc:location ->
  string option Location.loc ->
  module_type ->
  signature ->
  (Longident.t Location.loc * pattern option * inv_ctx * core_type) list ->
  module_expr

(** Generates the scaffolding for the [ExprToImpl] functor 
    (e.g. module type declarations) *)
val generate_functor :
  ctxt:Expansion_context.Deriver.t -> module_type_declaration -> structure
