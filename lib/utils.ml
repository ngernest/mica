open Ppxlib
open Ast_helper
open Ast_builder.Default
open StdLabels
open Equality 
open Lident 
open Miscellany
open Printers

(******************************************************************************)
(** {1 Working with [module_expr]s} *)

(** Constructs a [module_expr] from a string containing the module name *)
let module_expr_of_string ~(loc : Location.t) (str : string) : module_expr =
  pmod_ident ~loc (with_loc ~loc (Longident.parse str))

(** [let_open ~loc M e] creates the expression 
    [let open M in e], where [m] is some [module_expr] *)
let let_open ~(loc : Location.t) (m : module_expr) (e : expression) : expression
    =
  let mod_infos = open_infos ~loc ~expr:m ~override:Fresh in
  pexp_open ~loc mod_infos e

(** [let_open_twice ~loc M1 M2 e] produces the [expression] 
    [let open M1 in let open M2 in e] at location [loc],
    for [module_expr]s [M1] and [M2] *)
let let_open_twice ~(loc : Location.t) (m1 : module_expr) (m2 : module_expr)
  (e : expression) =
  let_open ~loc m1 (let_open ~loc m2 e)

(******************************************************************************)
(** {1 Utility functions for working with Ppxlib} *)

(** [pexp_ident_of_string x ~loc] creates the expression [Pexp_ident x]
    at location [loc] *)
let pexp_ident_of_string (x : string) ~(loc : Location.t) : expression =
  pexp_ident ~loc (lident_loc_of_string x ~loc)

(** [ppat_var_of_string x ~loc] creates the pattern [Ppat_var x] 
    at location [loc] *)
let ppat_var_of_string (x : string) ~(loc : Location.t) : pattern =
  ppat_var ~loc (with_loc x ~loc)

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

(** [get_type_varams td] extracts the type parameters 
    from the type declaration [td]
    - Type variables (e.g. ['a]) are instantiated with [int] *)
let get_type_params (td : type_declaration) : core_type list =
  List.map td.ptype_params ~f:(fun (core_ty, _) -> monomorphize core_ty)

(** "Inverse" typing context: maps types to variable names, implemented
   as an association list *)
type inv_ctx = (core_type * string) list

(** The empty "inverse" typing context *)
let empty_ctx : inv_ctx = []

(** [mk_fresh ~loc i ty] generates a fresh variable at location [loc] 
    that corresponds to the type [ty], with the (integer) index [i + 1] 
    used as a varname suffix 
    - We add 1 to [i] so that variable names are 1-indexed *)
let rec mk_fresh ~(loc : Location.t) (i : int) (ty : core_type) : pattern =
  let varname =
    match ty with
    | [%type: bool] -> "b"
    | [%type: char] -> "c"
    | [%type: string] -> "s"
    | [%type: unit] -> "u"
    | [%type: int] | [%type: 'a] -> "x"
    | [%type: expr] | [%type: t] | [%type: 'a t] -> "e"
    | { ptyp_desc; _ } -> (
      match ptyp_desc with
      | Ptyp_tuple _ -> "p"
      | Ptyp_arrow _ -> "f"
      | Ptyp_constr ({ txt; _ }, _) ->
        let tyconstr = string_of_lident txt in
        if String.equal tyconstr "list" then "lst"
          (* For unrecognized type constructors, just extract the first char of
             the type constructor's name *)
        else String.sub tyconstr ~pos:0 ~len:1
      | _ ->
        pp_core_type ty;
        failwith "TODO: [mk_fresh] not supported for types of this shape") in
  ppat_var ~loc (with_loc ~loc (varname ^ Int.to_string (i + 1)))

(** Turns a variable name [x] into [x'] *)
let rec add_prime (x : string) : string = x ^ "\'"

(** Extracts the variable name from a [Ppat_var] pattern 
    - Raises [Not_found] if the input pattern is not of the form [Ppat_var] *)
let get_varname ({ ppat_desc; _ } : pattern) : string =
  match ppat_desc with
  | Ppat_var { txt; _ } -> txt
  | _ -> raise Not_found

(** Takes [ty], the type of a [val] declaration in a signature,
    and returns the type of the arguments of the corresponding 
    constructor for the [expr] datatype. 

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
let rec get_cstr_arg_tys ?(is_arrow = false) (ty : core_type) : core_type list =
  let loc = ty.ptyp_loc in
  match monomorphize ty with
  | ty' when List.mem ty' ~set:(base_types ~loc) -> [ ty' ]
  | { ptyp_desc = Ptyp_constr ({ txt = lident; _ }, _); _ } as ty' ->
    let tyconstr = string_of_lident lident in
    if String.equal tyconstr abstract_ty_name then
      if is_arrow then [ [%type: expr] ] else []
    else [ ty' ]
  | { ptyp_desc = Ptyp_arrow (_, t1, t2); _ } ->
    get_cstr_arg_tys ~is_arrow:true t1 @ get_cstr_arg_tys ~is_arrow:true t2
  | { ptyp_desc = Ptyp_tuple tys; _ } ->
    List.concat_map ~f:(get_cstr_arg_tys ~is_arrow) tys
  | _ -> failwith "TODO: get_cstr_arg_tys"

(** Extracts the (monomorphized) return type of a type expression 
    (i.e. the rightmost type in an arrow type) *)
let rec get_ret_ty (ty : core_type) : core_type =
  let loc = ty.ptyp_loc in
  let ty_mono = monomorphize ty in
  if List.mem ty_mono ~set:(base_types ~loc) then ty_mono
  else
    match ty_mono.ptyp_desc with
    | Ptyp_constr _ | Ptyp_tuple _ | Ptyp_any | Ptyp_var _ -> ty_mono
    | Ptyp_arrow (_, _, t2) -> get_ret_ty t2
    | _ -> failwith "Type expression not supported by get_ret_ty"

(** Helper function: [get_cstr_args loc get_ty args] takes [args], 
    a list containing the {i representation} of constructor arguments, 
    applies the function [get_ty] to each element of [args] and produces 
    a formatted tuple of constructor arguments (using the [ppat_tuple] smart 
    constructor for the [pattern] type).  
    - Note that [args] has type ['a list], i.e. the representation of 
    constructor arguments is polymorphic -- this function is instantiated 
    with different types when called in [get_cstr_metadata] *)
let get_cstr_args ~(loc : Location.t) (get_ty : 'a -> core_type)
  (args : 'a list) : pattern * inv_ctx =
  let arg_tys : core_type list = List.map ~f:get_ty args in
  let arg_names : pattern list = List.mapi ~f:(mk_fresh ~loc) arg_tys in
  let gamma : inv_ctx =
    List.fold_left2
      ~f:(fun acc var_pat ty -> (ty, get_varname var_pat) :: acc)
      ~init:[] arg_names arg_tys in
  (ppat_tuple ~loc arg_names, gamma)

(** [find_exprs gamma] extracts all the variables with type [expr] from the 
    inverse typing context [gamma] *)
let find_exprs (gamma : inv_ctx) : string list =
  List.fold_left
    ~f:(fun acc (ty, var) ->
      match ty with
      | [%type: expr] -> var :: acc
      | _ -> acc)
    ~init:[] gamma

(** Takes a list of [constructor_declaration]'s and returns 
    a list consisting of 4-tuples of the form 
    (constructor name, constructor arguments, typing context, return type) *)
let get_cstr_metadata (cstrs : (constructor_declaration * core_type) list) :
  (Longident.t Location.loc * pattern option * inv_ctx * core_type) list =
  List.map cstrs ~f:(fun ({ pcd_name = { txt; loc }; pcd_args; _ }, ret_ty) ->
      let cstr_name = with_loc (Longident.parse txt) ~loc in
      match pcd_args with
      (* Constructors with no arguments *)
      | Pcstr_tuple [] -> (cstr_name, None, empty_ctx, ret_ty)
      (* N-ary constructors (where n > 0) *)
      | Pcstr_tuple arg_tys ->
        let (cstr_args, gamma) : pattern * inv_ctx =
          get_cstr_args ~loc Fun.id arg_tys in
        (cstr_name, Some cstr_args, gamma, ret_ty)
      | Pcstr_record arg_lbls ->
        let cstr_args, gamma =
          get_cstr_args ~loc (fun lbl_decl -> lbl_decl.pld_type) arg_lbls in
        (cstr_name, Some cstr_args, gamma, ret_ty))

(** Variant of [get_cstr_metadata] which returns 
    only a list of pairs containing constructor names & constructor args *)
let get_cstr_metadata_minimal (cstrs : constructor_declaration list) :
  (Longident.t Location.loc * pattern option) list =
  List.map cstrs ~f:(fun { pcd_name = { txt; loc }; pcd_args; _ } ->
      let cstr_name = with_loc (Longident.parse txt) ~loc in
      match pcd_args with
      (* Constructors with no arguments *)
      | Pcstr_tuple [] -> (cstr_name, None)
      (* N-ary constructors (where n > 0) *)
      | Pcstr_tuple arg_tys ->
        let (cstr_args, gamma) : pattern * inv_ctx =
          get_cstr_args ~loc Fun.id arg_tys in
        (cstr_name, Some cstr_args)
      | Pcstr_record arg_lbls ->
        let cstr_args, gamma =
          get_cstr_args ~loc (fun lbl_decl -> lbl_decl.pld_type) arg_lbls in
        (cstr_name, Some cstr_args))

(** Extracts the constructor name (along with its location) from 
    a constructor declaration *)
let get_cstr_name (cstr : constructor_declaration) : Longident.t Location.loc =
  let { txt; loc } = cstr.pcd_name in
  with_loc ~loc (Longident.parse txt)

(** Takes a [type_declaration] for an algebraic data type 
    and returns a list of (constructor name, constructor arguments) 
    - Raises an exception if the [type_declaration] doesn't correspond to an 
      algebraic data type *)
let get_cstrs_of_ty_decl (ty_decl : type_declaration) :
  (Longident.t Location.loc * pattern option) list =
  match ty_decl.ptype_kind with
  | Ptype_variant args -> get_cstr_metadata_minimal args
  | _ -> failwith "error: expected an algebraic data type definition"

(******************************************************************************)

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

(** Returns true the abstract type declaration in a [signature] 
    is parameterized (e.g. ['a t]), else returns [false] *)
let rec is_abs_ty_parameterized (sig_items : signature) : bool =
  List.fold_left
    ~f:(fun acc { psig_desc; _ } ->
      match psig_desc with
      | Psig_type (_rec_flag, ty_decls) -> (
        match ty_decls with
        | [] -> acc
        | _ ->
          list_or
          @@ List.map
               ~f:(fun { ptype_name; ptype_params; _ } ->
                 String.equal ptype_name.txt "t"
                 && not (list_is_empty ptype_params))
               ty_decls)
      | _ -> acc)
    ~init:false sig_items

(** [mk_valt_pat "x" ~loc] creates the pattern [ValT x], 
    consisting of the constructor [Valt] applied to the argument [x] 
    - The named argument [abs_ty_parameterized] represents whether the 
    abstract type [t] in the module signature is parameterized (e.g. ['a t]) *)
let mk_valt_pat ?(abs_ty_parameterized = false) (x : string) ~(loc : Location.t)
  : pattern =
  (* TODO: generalize this so that we can handle > 1 type parameter in abstract
     types *)
  let val_cstr = if abs_ty_parameterized then "ValIntT" else "ValT" in
  let var_ident = ppat_var_of_string x ~loc in
  ppat_construct ~loc
    (with_loc ~loc (Longident.parse val_cstr))
    (Some var_ident)

(** [get_match_arm ~loc expr_vars ~abs_ty_parameterized] returns the 
    match arms of the inner pattern match in [interp], e.g. 
    an expression of the form [ValIntT e]
    - The argument [expr_vars] is a list of variable names that 
    have type [expr]
    - The named argument [abs_ty_parameterized] represents whether the 
    abstract type [t] in the module signature is parameterized (e.g. ['a t]) *)
let get_match_arm (expr_vars : string list) ~(abs_ty_parameterized : bool)
  ~(loc : Location.t) : pattern =
  match expr_vars with
  | [] -> failwith "impossible: get_match_arm"
  | [ x ] -> mk_valt_pat ~loc ~abs_ty_parameterized (add_prime x)
  | _ ->
    let val_exprs : pattern list =
      List.map
        ~f:(fun x -> mk_valt_pat ~loc ~abs_ty_parameterized (add_prime x))
        expr_vars in
    ppat_tuple ~loc val_exprs

(** Creates the RHS of the inner case-stmt in [interp], for the special 
    case where we are dealing with a unary [value] constructor
    and a unary module function, e.g. [match e with ValInt x -> M.f x] 
    (In this example, [get_unary_case_rhs] produces the expression [M.f x])
    - [value_cstr] is the name of the constructor for the [value] type 
    - [expr_cstr] is the constructor for the [expr] type, which corresponds
    to a function inside the module with name [mod_name] 
    - [x] is the argument that will be applied to the module function *)
let get_unary_case_rhs (value_cstr : Longident.t Location.loc)
  (mod_name : string) (expr_cstr : Longident.t Location.loc) (x : string)
  ~(loc : Location.t) : expression =
  let mod_func = pexp_ident ~loc (add_lident_loc_prefix mod_name expr_cstr) in
  let mod_func_arg = pexp_ident_of_string (add_prime x) ~loc in
  let mod_func_app = [%expr [%e mod_func] [%e mod_func_arg]] in
  pexp_construct ~loc value_cstr (Some mod_func_app)

(** Variant of [get_unary_case_rhs] which handles the situation 
    when the RHS of the case statement is an n-ary function with 
    arguments [xs] *)
let get_nary_case_rhs (ret_ty_cstr : constructor_declaration)
  (mod_name : string) (expr_cstr : Longident.t Location.loc)
  (xs : expression list) ~loc : expression =
  let mod_func = pexp_ident ~loc (add_lident_loc_prefix mod_name expr_cstr) in
  let mod_func_app =
    pexp_apply ~loc mod_func (List.map ~f:(fun x -> (Nolabel, x)) xs) in
  let value_cstr = get_cstr_name ret_ty_cstr in
  pexp_construct ~loc value_cstr (Some mod_func_app)

(** [update_expr_arg_names expr_args args] replaces each variable [x] in 
    [expr_args] if [x'] (the variable with a prime added) is in [expr_args] *)
let update_expr_arg_names (expr_args : string list) (args : string list) :
  string list =
  List.map args ~f:(fun x ->
      if List.mem (add_prime x) ~set:expr_args then add_prime x else x)

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
    let xs =
      List.map expr_vars ~f:(fun x ->
          [%expr interp [%e pexp_ident_of_string x ~loc]]) in
    if List.length xs = 1 then List.hd xs else post xs

(** Takes a [type_declaration] and returns a pair of the form 
    [(<type_name, list_of_type_parameters)] *)
let get_ty_name_and_params ({ ptype_name; ptype_params; _ } : type_declaration)
  : string * core_type list =
  let ty_params = List.map ~f:fst ptype_params in
  (ptype_name.txt, ty_params)

(** Takes a module signature and returns a list containing pairs of the form
    [(<type_name>, <list_of_type_parameters>)]. The list is ordered based on
    the order of appearance of the type declarations in the signature.  *)
let get_ty_decls_from_sig (sig_items : signature) :
  (string * core_type list) list =
  List.fold_left sig_items ~init:[] ~f:(fun acc { psig_desc; _ } ->
      match psig_desc with
      | Psig_type (_, ty_decls) ->
        List.map ~f:get_ty_name_and_params ty_decls :: acc
      | _ -> acc)
  |> List.concat |> List.rev

(* -------------------------------------------------------------------------- *)
(*      Helpers for deriving monadic code (currently unused)                  *)
(* -------------------------------------------------------------------------- *)

let monadic_bindop ~(loc : Location.t) (x : string) (exp : expression) :
  binding_op =
  let op = with_loc ~loc "bind" in
  let pat = ppat_var ~loc (with_loc ~loc x) in
  binding_op ~loc ~op ~pat ~exp

let let_monadic_bind ~(loc : Location.t) (x : string) (e1 : expression)
  (e2 : expression) : expression =
  let letop = letop ~let_:(monadic_bindop ~loc x e1) ~ands:[] ~body:e2 in
  pexp_letop ~loc letop
