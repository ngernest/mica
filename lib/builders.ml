open Ppxlib
open Ast_builder.Default
open StdLabels
open Lident
open Names
open Printers
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

(** [mk_error ~local ~global msg] creates an error extension node, 
    associated with an element in the AST at the location [local],
    and reports the error message [msg] at the location [global] *)
let mk_error ~(local : Location.t) ~(global : Location.t) msg : structure_item =
  let ext = Location.error_extensionf ~loc:local msg in
  pstr_extension ~loc:global ext []

(** [attr loc name] creates an attribute called [name] at [loc] *)
let attr ~(loc : Location.t) ~(name : string) : attribute =
  attribute ~loc ~name:{ txt = "deriving"; loc }
    ~payload:
      (PStr
         [ { pstr_desc =
               Pstr_eval (pexp_ident ~loc { txt = Lident name; loc }, []);
             pstr_loc = loc
           }
         ])

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

(** Takes a list [xs] of [expressions] and builds a list literal 
    containing the elements of [xs]. Formally, if [xs] consists of
    elements [x1; x2; ...], this function builds the expression 
    [x1 :: x2 :: ... :: []] consisting of successive Cons-cells. 
    However, the list is pretty-printed as the list literal [[x1; x2; ...]]. 
    - Note: this function isn't available in [Ppxlib.Ast_builder] and there 
    is no [pexp_list] expression type in [Parsetree], so we have to 
    implement this function ourselves. *)
let pexp_list ~(loc : Location.t) (xs : expression list) : expression =
  let nil = pexp_construct ~loc (lident_loc_of_string ~loc "[]") None in
  match xs with
  | [] -> nil
  | _ ->
    (* Build the list by cons-ing elements in a right-associative manner *)
    List.fold_right
      ~f:(fun acc ({ pexp_loc; _ } as x) ->
        pexp_construct ~loc:pexp_loc
          (lident_loc_of_string ~loc:pexp_loc "::")
          (pexp_tuple_opt ~loc:pexp_loc [ acc; x ]))
      ~init:nil xs
