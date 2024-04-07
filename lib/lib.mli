open Ppxlib

(** Takes [ty], the type of a [val] declaration in a signature,
    and returns the type of the arguments of the corresponding 
    constructor for the [expr] datatype. 

    For the [Set] module signature example,
    - [val empty : 'a t] corresponds to the 0-arity [Empty] constructor
    - [val is_empty : 'a t -> bool] corresponds to [Is_empty of expr * bool] 
    - Monormorphic primitive types are preserved. 

    The [is_arrow] optional 
    named argument specifies whether [ty] is an arrow type: if yes, then 
    references to abstract types should be replaced with [expr], otherwise
    an occurrence of an abstract type in an non-arrow type 
    (e.g. [val empty : 'a t]) should be ignored (so [val empty : 'a t] 
    corresponds to the 0-arity constructor [Empty]). *)
val get_constructor_arg_tys : ?is_arrow:bool -> core_type -> core_type list

(** Extracts the (monomorphized) return type of a type expression 
    (i.e. the rightmost type in an arrow type) *)
val get_ret_ty : core_type -> core_type

(** Walks over all the [val ...] declarations in a module signature
    and creates the corresponding definition of the [expr] ADT *)
val mk_expr_constructors : signature -> constructor_declaration list

(** Extracts the unique return types of all [val] declarations within a 
    module signature *)
val uniq_ret_tys : signature -> core_type list

(** Helper function for creating the constructors of the [ty] and [value] 
    algebraic data types 
    - The argument [sig_items] contains the contents of a module signature
    - [~f] is a function that specifies how to turn a [core_type] into a 
    [constructor_declaration] *)
val mk_constructor_aux :
  signature ->
  f:(core_type -> constructor_declaration) ->
  constructor_declaration list

(** Constructs the definition of the [ty] algebraic data type
based on the unique return types of all [val] declarations within 
the module signature *)
val mk_ty_constructors : signature -> constructor_declaration list

(** Constructs the definition of the [value] algebraic data type
    based on the inhabitants of the [ty] ADT *)
val mk_val_constructors : signature -> constructor_declaration list

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
    [get_expr_constructors] produces [expr] constructor names & arguments
    that match the declarations in the module signature *)
val get_expr_constructors :
  module_type ->
  (Longident.t Location.loc * pattern option * Utils.inv_ctx) list

(** Creates the body of the inner case-statement inside [interp]
  - NB: [gamma] is the "inverse typing context" which maps types 
    to variable names *)
val mk_interp_case_rhs : 
  loc:location -> 
  mod_name:string -> 
  ?abs_ty_parameterized:bool -> 
  Longident.t Location.loc ->
  pattern option -> 
  gamma:Utils.inv_ctx -> 
  expression 

(** Creates the definition for the [interp] function 
    (contained inside the body of the [ExprToImpl] functor) 
    - The argument [expr_cstrs] is a list containing the 
    names & arg types of the constructors for the [expr] algebraic data type *)
val mk_interp :
  loc:location ->
  module_type ->
  ?abs_ty_parameterized:bool ->
  (Longident.t Location.loc * pattern option * Utils.inv_ctx) list ->
  structure_item

(** Creates the body of the [ExprToImpl] functor *)
val mk_functor :
  loc:location ->
  string option Location.loc ->
  module_type ->
  signature ->
  (Longident.t Location.loc * pattern option * Utils.inv_ctx) list ->
  module_expr

(** Generates the scaffolding for the [ExprToImpl] functor 
    (e.g. module type declarations) *)
val generate_functor :
  ctxt:Expansion_context.Deriver.t -> module_type_declaration -> structure
