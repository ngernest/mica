open Ppxlib

(** [mk_adt ~loc ~name constructors] creates the definition of 
    an algebraic data type called [name] at location [loc] 
    with the specified [constructors] *)
val mk_cstr :
  name:string ->
  loc:Location.t ->
  arg_tys:core_type list ->
  constructor_declaration

(** [mk_cstr ~name ~loc arg_tys] creates a constructor with the [name] 
    for an algebraic data type at the location [loc] with 
    argument types [arg_tys] *)
val mk_adt :
  loc:Location.t ->
  name:string ->
  cstrs:constructor_declaration list ->
  type_declaration

(** [mk_valt_pat "x" ~loc] creates the pattern [ValT x], 
    consisting of the constructor [Valt] applied to the argument [x] 
    - The named argument [abs_ty_parameterized] represents whether the 
    abstract type [t] in the module signature is parameterized (e.g. ['a t]) *)
val mk_valt_pat :
  ?abs_ty_parameterized:bool -> string -> loc:Location.t -> pattern

(** Makes the scrutinees for the inner case-stmt in [interp]. 
    - [expr_vars] is a list of variables that have type [expr]. This list 
    must be non-empty, otherwise [mk_scrutinees] throws an exception.
    - [post] is post-processing function to be applied when [expr_vars] 
    has length >= 2 after being transformed into an [expression list] *)
val mk_scrutinees :
  string list ->
  post:(expression list -> expression) ->
  loc:Location.t ->
  expression

(** [deriving_attribute ~loc expr] creates a [[@@deriving ...]] attribute 
    with the payload [expr] at location [loc] *)  
val deriving_attribute : loc:Location.t -> expression -> attribute  