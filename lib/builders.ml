open Ppxlib
open Ast_builder.Default
open StdLabels
open Miscellany

(** [mk_adt ~loc ~name constructors] creates the definition of 
    an algebraic data type called [name] at location [loc] 
    with the specified [constructors] *)
let mk_adt ~(loc : Location.t) ~(name : string)
  ~(cstrs : constructor_declaration list) : type_declaration =
  type_declaration ~loc
    ~name:{ txt = name; loc } (* Name of type *)
    ~cstrs:[] (* Type constraints, not needed here *)
    ~params:[] (* Type parameters *)
    ~kind:(Ptype_variant cstrs) ~private_:Public
    ~manifest:None (* RHS of [type t =...], doesn't apply here *)

(** [mk_cstr ~name ~loc arg_tys] creates a constructor with the [name] 
    for an algebraic data type at the location [loc] with 
    argument types [arg_tys] *)
let mk_cstr ~(name : string) ~(loc : Location.t) ~(arg_tys : core_type list) :
  constructor_declaration =
  { (* Constructor name *)
    pcd_name = { txt = name; loc };
    (* Type variables *)
    pcd_vars = [];
    (* Constructor arguments *)
    pcd_args = Pcstr_tuple arg_tys;
    (* Constructor result *)
    pcd_res = None;
    (* Location of the type *)
    pcd_loc = loc;
    (* Any PPXes attached to the type *)
    pcd_attributes = []
  }

(** [mk_valt_pat "x" ~loc] creates the pattern [ValT x], 
    consisting of the constructor [Valt] applied to the argument [x] 
    - The named argument [abs_ty_parameterized] represents whether the 
    abstract type [t] in the module signature is parameterized (e.g. ['a t]) *)
let mk_valt_pat ?(abs_ty_parameterized = false) (x : string) ~(loc : Location.t)
  : pattern =
  (* TODO: generalize this so that we can handle > 1 type parameter in abstract
     types *)
  let val_cstr = if abs_ty_parameterized then "ValIntT" else "ValT" in
  let var_ident = pvar x ~loc in
  ppat_construct ~loc
    (with_loc ~loc (Longident.parse val_cstr))
    (Some var_ident)

(** Makes the scrutinees for the inner case-stmt in [interp]. 
    - [expr_vars] is a list of variables that have type [expr]. This list 
    must be non-empty, otherwise [mk_scrutinees] throws an exception.
    - [post] is post-processing function to be applied when [expr_vars] 
    has length >= 2 after being transformed into an [expression list] *)
let mk_scrutinees (expr_vars : string list)
  ~(post : expression list -> expression) ~(loc : Location.t) : expression =
  match expr_vars with
  | [] -> failwith "impossible: mk_scrutinees"
  | _ ->
    let xs = List.map expr_vars ~f:(fun x -> [%expr interp [%e evar x ~loc]]) in
    if List.length xs = 1 then List.hd xs else post xs

(** [deriving_attribute ~loc ~name] creates a [[@@deriving ...]] attribute 
    with the payload [name] at location [loc] *)
let deriving_attribute ~(loc : Location.t) (expr : expression) : attribute =
  let payload = PStr [%str [%e expr]] in

  attribute ~loc ~name:{ txt = "deriving"; loc } ~payload
