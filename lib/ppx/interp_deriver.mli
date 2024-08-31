open Ppxlib
open Utils

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
  ctxt:Expansion_context.Deriver.t -> module_type_declaration -> structure_item
