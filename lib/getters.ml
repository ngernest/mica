open Ppxlib
open StdLabels
open Ast_helper
open Ast_builder.Default
open Miscellany
open Printers
open Names
open Lident
open Inv_ctx
open Builders

(******************************************************************************)
(** {1 Working with constructors for algebraic data types} *)

(** Extracts the variable name from a [Ppat_var] pattern 
  - Raises [Not_found] if the input pattern is not of the form [Ppat_var] *)
let get_varname ({ ppat_desc; _ } : pattern) : string =
  match ppat_desc with
  | Ppat_var { txt; _ } -> txt
  | _ -> raise Not_found

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
let rec get_arg_tys_of_expr_cstr ?(is_arrow = false) (ty : core_type)
  (abs_ty_names : string list) : core_type list =
  let loc = ty.ptyp_loc in
  match monomorphize ty with
  | ty' when List.mem ty' ~set:(base_types ~loc) -> [ ty' ]
  | { ptyp_desc = Ptyp_constr ({ txt = lident; _ }, _); _ } as ty' ->
    let tyconstr = string_of_lident lident in
    if List.mem tyconstr ~set:abs_ty_names then
      if is_arrow then [ [%type: expr] ] else []
    else [ ty' ]
  | { ptyp_desc = Ptyp_arrow (_, t1, t2); _ } ->
    get_arg_tys_of_expr_cstr ~is_arrow:true t1 abs_ty_names
    @ get_arg_tys_of_expr_cstr ~is_arrow:true t2 abs_ty_names
  | { ptyp_desc = Ptyp_tuple tys; _ } ->
    List.concat_map
      ~f:(fun ty -> get_arg_tys_of_expr_cstr ~is_arrow ty abs_ty_names)
      tys
  | _ -> failwith "TODO: get_arg_tys_of_expr_cstr"

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
  let arg_names : pattern list = List.map ~f:(mk_fresh_pvar ~loc) arg_tys in
  let gamma : inv_ctx =
    List.fold_left2
      ~f:(fun acc var_pat ty -> (ty, get_varname var_pat) :: acc)
      ~init:[] arg_names arg_tys in
  (ppat_tuple ~loc arg_names, gamma)

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

(** Computes the arity of a constructor for an algebraic data type *)
let get_cstr_arity (cstr : constructor_declaration) : int =
  match cstr.pcd_args with
  | Pcstr_tuple xs -> List.length xs
  | Pcstr_record lbls -> List.length lbls

(** Retrieves the argument types of a constructor for an algebraic data type *)  
let get_cstr_arg_tys (cstr : constructor_declaration) : core_type list = 
  match cstr.pcd_args with 
  | Pcstr_tuple tys -> tys 
  | Pcstr_record lbls -> List.map ~f:(fun lbl -> lbl.pld_type) lbls 

(******************************************************************************)
(** {1 Working with type parameters & type declarations} *)

(** [get_type_varams td] extracts the type parameters 
    from the type declaration [td]
    - Type variables (e.g. ['a]) are instantiated with [int] *)
let get_type_params (td : type_declaration) : core_type list =
  List.map td.ptype_params ~f:(fun (core_ty, _) -> monomorphize core_ty)

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

(** Retrieves all the abstract types from a signature as a list of 
    [type_declaration]s *)
let get_abs_tys_from_sig (sig_items : signature) : type_declaration list =
  List.fold_left
    ~f:(fun acc { psig_desc; _ } ->
      match psig_desc with
      | Psig_type (_, ty_decls) -> acc @ ty_decls
      | _ -> acc)
    ~init:[] sig_items

(** Retrieves the names of all the abstract types in a signature *)
let get_abs_ty_names (sig_items : signature) : string list =
  let abs_tys = get_abs_tys_from_sig sig_items in
  List.map ~f:(fun abs_ty -> no_loc abs_ty.ptype_name) abs_tys

(******************************************************************************)
(** {1 Working with pattern matches} *)

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

(** Creates the RHS of the inner pattern-match in [interp], for the special 
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
  let mod_func_arg = evar (add_prime x) ~loc in
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
