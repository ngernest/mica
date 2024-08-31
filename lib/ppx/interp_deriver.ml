open Ppxlib
open Ast_helper
open Ast_builder.Default
open StdLabels
open Utils
open Type_deriver

(******************************************************************************)

(** {1 Generator for Functors} *)

type interp_case_rhs_params =
  { loc : Location.t;
    mod_name : string;
    abs_ty_parameterized : bool;
    expr_cstr : Longident.t Location.loc;
    args : pattern option;
    gamma : inv_ctx;
    ret_ty : core_type
  }

(** Creates the body of the inner case-statement inside [interp].
  - [gamma] is the "inverse typing context" which maps types 
    to variable names
  - [expr_cstr] is the constructor of the [expr] type 
  - [ret_ty] is the designated return type of the expression being interpreted *)
let mk_interp_case_rhs (params : interp_case_rhs_params) : expression =
  let { loc; mod_name; abs_ty_parameterized; expr_cstr; args; gamma; ret_ty } =
    params in
  (* The constructor of the [value] type for [ret_ty] *)
  let ret_ty_cstr = mk_val_cstr ret_ty in
  (* The constructor name *)
  let value_cstr = get_cstr_name ret_ty_cstr in
  (* Append the module name to the expression we're interepreting *)
  let mod_item = pexp_ident ~loc (add_lident_loc_prefix mod_name expr_cstr) in
  match args with
  (* Nullary constructors *)
  | None -> pexp_construct ~loc value_cstr (Some mod_item)
  (* Unary constructors *)
  | Some { ppat_desc = Ppat_var { txt = x; loc }; _ } ->
    let expr_vars = find_exprs gamma in
    let func_arg_ident = evar x ~loc in
    (* If [x] doesn't have type [expr]: regular function application *)
    if not (List.mem x ~set:expr_vars) then
      pexp_construct ~loc value_cstr
        (Some [%expr [%e mod_item] [%e func_arg_ident]])
    else
      (* Otherwise, recursively call [interp] on the variable of type [expr] *)
      let scrutinee = [%expr interp [%e func_arg_ident]] in
      let match_arm = get_match_arm ~loc [ x ] ~abs_ty_parameterized in
      let match_rhs = get_unary_case_rhs value_cstr mod_name expr_cstr x ~loc in
      [%expr
        match [%e scrutinee] with
        | [%p match_arm] -> [%e match_rhs]
        | _ -> failwith "impossible: unary constructor"]
  (* n-ary constructors (n > 1) *)
  | Some { ppat_desc = Ppat_tuple xs; ppat_loc; _ } ->
    let vars = List.map ~f:get_varname xs in
    let expr_vars = find_exprs gamma in
    (* If there are no args of type [expr]: regular function application *)
    if list_is_empty expr_vars then
      let func_args = List.map ~f:(fun x -> (Nolabel, evar ~loc x)) vars in
      pexp_construct ~loc value_cstr (Some (pexp_apply ~loc mod_item func_args))
    else
      (* Recursively call [interp] on all variables that have type [expr] *)
      let match_arm =
        get_match_arm ~loc:ppat_loc expr_vars ~abs_ty_parameterized in
      let scrutinees = mk_scrutinees expr_vars ~post:(pexp_tuple ~loc) ~loc in
      let func_args =
        List.map
          ~f:(fun x -> evar x ~loc)
          (update_expr_arg_names (List.map ~f:add_prime expr_vars) vars) in
      let match_rhs =
        get_nary_case_rhs ret_ty_cstr mod_name expr_cstr func_args ~loc in
      [%expr
        match [%e scrutinees] with
        | [%p match_arm] -> [%e match_rhs]
        | _ -> failwith "impossible: n-ary constructor"]
  | Some pat ->
    printf "cstr = %s\n" (string_of_lident expr_cstr.txt);
    printf "pat = ";
    Pprintast.pattern Format.err_formatter pat;
    printf "\n";
    failwith "TODO: catch all case of mk_interp_case_rhs"

(** Creates the definition for the [interp] function 
    (contained inside the body of the [Interpret] functor) 
    - The argument [expr_cstrs] is a list containing the 
    names & arg types of the constructors for the [expr] algebraic data type, 
    along with [inv_ctx], an "inverse typing context" that maps [core_type]'s 
    to variables with that type
    - The optional argument [abs_ty_parameterized] represents whether 
    the abstract type [t] in the module signature is parameterized (e.g. ['a t]) 
    or not *)
let mk_interp ~(loc : location) ?(abs_ty_parameterized = false)
  (expr_cstrs :
    (Longident.t Location.loc * pattern option * inv_ctx * core_type) list) :
  structure_item =
  (* Each [expr] constructor corresponds to the LHS of a pattern match case *)
  let cases : case list =
    List.map expr_cstrs ~f:(fun (expr_cstr, args, gamma, ret_ty) ->
      let lhs : pattern = ppat_construct ~loc expr_cstr args in
      let params : interp_case_rhs_params =
        { loc;
          gamma;
          mod_name = "M";
          abs_ty_parameterized;
          expr_cstr;
          args;
          ret_ty
        } in
      let rhs : expression = mk_interp_case_rhs params in
      case ~lhs ~guard:None ~rhs) in
  let arg_ident = evar "e" ~loc in
  let func_body : expression = pexp_match ~loc arg_ident cases in
  [%stri let rec interp e = [%e func_body]]

(** Creates the body of the [Interpret] functor *)
let mk_functor ~(loc : location) (arg_name : label option with_loc)
  (mod_ty : module_type) (sig_items : signature)
  (expr_cstrs :
    (Longident.t Location.loc * pattern option * inv_ctx * core_type) list) :
  module_expr =
  (* [open M] declaration *)
  let m_ident : Longident.t Location.loc =
    { txt = Longident.parse (Option.value arg_name.txt ~default:"M"); loc }
  in
  let m_expr : module_expr = pmod_ident ~loc m_ident in
  let open_m = open_infos ~loc ~expr:m_expr ~override:Fresh in
  let open_decl : structure_item = pstr_open ~loc open_m in
  (* Declaration for the [value] ADT *)
  let val_adt : type_declaration =
    mk_adt ~loc ~name:"value" ~cstrs:(mk_val_cstrs sig_items) in
  let val_adt_decl : structure_item = pstr_type ~loc Recursive [ val_adt ] in
  let abs_ty_parameterized : bool = is_abs_ty_parameterized sig_items in
  let interp_fun_defn = mk_interp ~loc ~abs_ty_parameterized expr_cstrs in
  let functor_body : structure_item list =
    [%str
      [%%i open_decl]
      [%%i val_adt_decl]
      [%%i interp_fun_defn]] in
  let functor_expr : module_expr =
    { pmod_desc = Pmod_structure functor_body;
      pmod_loc = loc;
      pmod_attributes = []
    } in
  pmod_functor ~loc (Named (arg_name, mod_ty)) functor_expr

(** Generates the scaffolding for the [Interpret] functor 
    (e.g. module type declarations) *)
let generate_functor ~(ctxt : Expansion_context.Deriver.t)
  (mt : module_type_declaration) : structure_item =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  match mt with
  | { pmtd_type = Some mod_type; pmtd_name; _ } -> (
    let new_name = { txt = Some "M"; loc } in
    let mod_type_alias = pmty_ident ~loc { txt = Lident pmtd_name.txt; loc } in
    match mod_type.pmty_desc with
    | Pmty_signature sig_items ->
      (* Obtain the constructors for the [expr] datatype based on the module
         type signature, then pass them onto [mk_functor] when building the body
         of the functor *)
      let expr_cstrs = get_expr_cstrs mod_type in
      let functor_expr =
        mk_functor ~loc new_name mod_type_alias sig_items expr_cstrs in
      let mod_binding =
        module_binding ~loc
          ~name:{ txt = Some "Interpret"; loc }
          ~expr:functor_expr in
      { pstr_desc = Pstr_module mod_binding; pstr_loc = loc }
    | _ ->
      mk_error_pstr ~local:mod_type.pmty_loc ~global:loc
        "Expected a module type expression that was a signature")
  | { pmtd_type = None; _ } ->
    Location.raise_errorf ~loc
      "Can't derive for expressions that aren't module type declarations"
