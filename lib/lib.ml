open Ppxlib
open Ast_helper
open Ast_builder.Default
open StdLabels
open Utils

(******************************************************************************)
(** {1 Generator for Auxiliary Datatypes} *)

(** Walks over all the [val ...] declarations in a module signature
    and creates the corresponding definition of the [expr] ADT *)
let mk_expr_cstrs (sig_items : signature) : constructor_declaration list =
  List.rev
  @@ List.fold_left sig_items ~init:[] ~f:(fun acc { psig_desc; psig_loc; _ } ->
       match psig_desc with
       | Psig_type (rec_flag, type_decls) -> []
       | Psig_value { pval_name; pval_type; pval_loc; _ } ->
         let name = String.capitalize_ascii pval_name.txt in
         (* Exclude the return type of the function from the list of arg types
            for the [expr] constructor *)
         let arg_tys = remove_last (get_cstr_arg_tys pval_type) in
         (* Return type of the function *)
         let ret_ty = get_ret_ty pval_type in
         mk_cstr ~name ~loc:pval_loc ~arg_tys :: acc
       | Psig_attribute attr -> failwith "TODO: handle attribute [@@@id]"
       | Psig_extension (ext, attrs) -> failwith "TODO: handle extensions"
       | _ ->
         failwith
           "TODO: not sure how to handle other kinds of [signature_item_desc]")

(** Extracts the unique return types of all [val] declarations within a 
    module signature *)
let uniq_ret_tys (sig_items : signature) : core_type list =
  List.rev
  @@ List.fold_left sig_items ~init:[] ~f:(fun acc { psig_desc; psig_loc; _ } ->
       match psig_desc with
       | Psig_value { pval_type; _ } ->
         let ty = get_ret_ty pval_type in
         if List.mem ty ~set:acc then acc else ty :: acc
       | _ -> acc)

(** Helper function for creating the constructors of the [ty] and [value] 
    algebraic data types 
    - The argument [sig_items] contains the contents of a module signature
    - [~f] is a function that specifies how to turn a [core_type] into a 
    [constructor_declaration] *)
let mk_cstr_aux (sig_items : signature)
  ~(f : core_type -> constructor_declaration) : constructor_declaration list =
  let ret_tys = uniq_ret_tys sig_items in
  let uniq_ret_tys =
    List.sort_uniq ret_tys ~cmp:(fun t1 t2 ->
      String.compare (string_of_core_ty t1) (string_of_core_ty t2)) in
  List.map uniq_ret_tys ~f

(** Constructs the definition of the [ty] algebraic data type
    based on the unique return types of all [val] declarations within 
    the module signature *)
let mk_ty_cstrs (sig_items : signature) : constructor_declaration list =
  mk_cstr_aux sig_items ~f:(fun ty ->
    mk_cstr ~name:(string_of_core_ty ty) ~loc:ty.ptyp_loc ~arg_tys:[])

(** [mk_val_cstr ty] constructors the corresponding constructor declaration
    for the [value] datatype, given some [core_type] [ty]
    - e.g. if [ty = Int], [mk_val_cstr] returns the declaration for 
      the [ValInt] constructor *)
let mk_val_cstr (ty : core_type) : constructor_declaration =
  mk_cstr ~name:("Val" ^ string_of_core_ty ty) ~loc:ty.ptyp_loc ~arg_tys:[ ty ]

(** Constructs the definition of the [value] algebraic data type
    based on the inhabitants of the [ty] ADT *)
let mk_val_cstrs (sig_items : signature) = mk_cstr_aux sig_items ~f:mk_val_cstr

(** Walks over a module signature definition and extracts the 
    abstract type declaration, producing the definition 
    the [expr] and [ty] algebraic data types *)
let generate_types_from_sig ~(ctxt : Expansion_context.Deriver.t)
  (mt : module_type_declaration) : structure_item list =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  match mt with
  | { pmtd_type = Some mod_type; pmtd_name; pmtd_loc; _ } -> (
    match mod_type with
    | { pmty_desc = Pmty_signature sig_items; pmty_loc; _ } -> (
      match sig_items with
      | [] ->
        [ mk_error ~local:pmtd_loc ~global:loc "Module sig can't be empty" ]
      | _ ->
        let expr_td =
          mk_adt ~loc ~name:"expr" ~cstrs:(mk_expr_cstrs sig_items) in
        let ty_td = mk_adt ~loc ~name:"ty" ~cstrs:(mk_ty_cstrs sig_items) in
        [ pstr_type ~loc Recursive [ expr_td ];
          pstr_type ~loc Recursive [ ty_td ]
        ])
    | _ -> failwith "TODO: other case for mod_type")
  | { pmtd_type = None; pmtd_loc; pmtd_name; _ } ->
    [ mk_error ~local:pmtd_loc ~global:loc
        "Can't derive for expressions that aren't module type declarations"
    ]

(** Instantiates the PPX deriver for [expr]s *)
let type_generator :
  (structure_item list, module_type_declaration) Deriving.Generator.t =
  Deriving.Generator.V2.make_noarg generate_types_from_sig

(******************************************************************************)

(** {1 Generator for Functors} *)

(** Helper function: given [mod_ty], a module signature,
    [get_expr_cstrs] produces [expr] constructor names & arguments
    that match the declarations in the module signature *)
let get_expr_cstrs (mod_ty : module_type) :
  (Longident.t Location.loc * pattern option * inv_ctx) list =
  match mod_ty.pmty_desc with
  | Pmty_signature sig_items -> get_cstr_metadata (mk_expr_cstrs sig_items)
  | _ -> failwith "TODO: get_expr_cstrs"

(** TODO: implement a version of [mk_valt_pat] that produces *)

(** Creates the body of the inner case-statement inside [interp]
  - NB: [gamma] is the "inverse typing context" which maps types 
    to variable names *)
let mk_interp_case_rhs ~(loc : location) ~(mod_name : string)
  ?(abs_ty_parameterized = false) (cstr : Longident.t Location.loc)
  (args : pattern option) ~(gamma : inv_ctx) : expression =
  match args with
  (* Constructors with no arguments *)
  | None ->
    (* TODO: need to produce an application of the [ValIntT] constructor *)
    pexp_ident ~loc (add_lident_loc_prefix mod_name cstr)
  (* Constructors with arity n, where n > 0 *)
  | Some { ppat_desc = Ppat_tuple xs; _ } ->
    let vars : string list = List.map ~f:get_varname xs in
    let expr_vars : string list = find_exprs gamma in
    let match_arm : pattern =
      get_match_arm ~loc expr_vars ~abs_ty_parameterized in
    let scrutinees : expression =
      match expr_vars with
      | [] ->
        failwith
          {| impossible: 
          can't call [mk_interp_case_rhs] with no arguments of type [expr] |}
      | [ x ] ->
        (* [match interp x with ...] *)
        let ident : expression = pexp_ident_of_string x ~loc in
        pexp_apply ~loc [%expr interp] [ (Nolabel, ident) ]
      | _ ->
        (* [match (interp y1, interp y2, ...) with ...] *)
        let app_exprs : expression list =
          List.map
            ~f:(fun var ->
              pexp_apply ~loc [%expr interp]
                [ (Nolabel, pexp_ident_of_string ~loc var) ])
            expr_vars in
        pexp_tuple ~loc app_exprs in
    (* TODO: figure out how to generate the body of this case stmt *)
    [%expr
      match [%e scrutinees] with
      | [%p match_arm] -> failwith "TODO: finish RHS"
      | _ -> failwith "impossible"]
  (* Constructors with one single argument *)
  | Some { ppat_desc = Ppat_var x; _ } ->
    let ident : expression = pexp_ident_of_string x.txt ~loc in
    let scrutinee : expression =
      pexp_apply ~loc [%expr interp] [ (Nolabel, ident) ] in
    let match_arm : pattern =
      get_match_arm ~loc [ x.txt ] ~abs_ty_parameterized in
    (* TODO: figure out how to generate the body of this case stmt *)
    [%expr
      match [%e scrutinee] with
      | [%p match_arm] -> failwith "TODO: finish RHS"
      | _ -> failwith "impossible"]
  | Some pat ->
    Stdio.printf "cstr = %s\n" (string_of_lident cstr.txt);
    Stdio.printf "pat = ";
    Pprintast.pattern Format.err_formatter pat;
    Stdio.printf "\n";
    failwith "TODO: catch all case of mk_interp_case_rhs"

(** Creates the definition for the [interp] function 
    (contained inside the body of the [ExprToImpl] functor) 
    - The argument [expr_cstrs] is a list containing the 
    names & arg types of the constructors for the [expr] algebraic data type, 
    along with [inv_ctx], an "inverse typing context" that maps [core_type]'s 
    to variables with that type
    - The optional argument [abs_ty_parameterized] represents whether 
    the abstract type [t] in the module signature is parameterized (e.g. ['a t]) 
    or not *)
let mk_interp ~(loc : location) (mod_ty : module_type)
  ?(abs_ty_parameterized = false)
  (expr_cstrs : (Longident.t Location.loc * pattern option * inv_ctx) list) :
  structure_item =
  (* [arg_str] denotes the argument to [interp] *)
  let arg_str = "e" in
  let arg_ident : expression =
    pexp_ident ~loc (with_loc (Lident arg_str) ~loc) in
  let func_name_pat : pattern = ppat_var ~loc { txt = "interp"; loc } in
  let func_arg : pattern = ppat_var ~loc { txt = arg_str; loc } in
  (* Each [expr] constructor corresponds to the LHS of a pattern match case *)
  let cases : case list =
    List.map expr_cstrs ~f:(fun (cstr, args, gamma) ->
      let lhs : pattern = ppat_construct ~loc cstr args in
      let rhs : expression =
        mk_interp_case_rhs ~loc ~gamma ~mod_name:"M" ~abs_ty_parameterized cstr
          args in
      case ~lhs ~guard:None ~rhs) in
  let func_body : expression = pexp_match ~loc arg_ident cases in
  let func_binding : expression =
    pexp_fun ~loc Nolabel None func_arg func_body in
  let func_defn : value_binding =
    value_binding ~loc ~pat:func_name_pat ~expr:func_binding in
  pstr_value ~loc Recursive [ func_defn ]

(** Creates the body of the [ExprToImpl] functor *)
let mk_functor ~(loc : location) (arg_name : label option with_loc)
  (mod_ty : module_type) (sig_items : signature)
  (expr_cstrs : (Longident.t Location.loc * pattern option * inv_ctx) list) :
  module_expr =
  (* [include M] declaration *)
  let m_ident : Longident.t Location.loc =
    { txt = Longident.parse (Option.value arg_name.txt ~default:"M"); loc }
  in
  let m_expr : module_expr = pmod_ident ~loc m_ident in
  let include_decl : structure_item =
    pstr_include ~loc (include_infos ~loc m_expr) in

  (* Declaration for the [value] ADT *)
  let val_adt : type_declaration =
    mk_adt ~loc ~name:"value" ~cstrs:(mk_val_cstrs sig_items) in
  let val_adt_decl : structure_item = pstr_type ~loc Recursive [ val_adt ] in

  let abs_ty_parameterized : bool = is_abs_ty_parameterized sig_items in

  (* Assembling all the components of the functor *)
  let functor_body : structure_item list =
    [ include_decl;
      val_adt_decl;
      mk_interp ~loc mod_ty ~abs_ty_parameterized expr_cstrs
    ] in
  let functor_expr : module_expr =
    { pmod_desc = Pmod_structure functor_body;
      pmod_loc = loc;
      pmod_attributes = []
    } in
  pmod_functor ~loc (Named (arg_name, mod_ty)) functor_expr

(** Generates the scaffolding for the [ExprToImpl] functor 
    (e.g. module type declarations) *)
let generate_functor ~ctxt (mt : module_type_declaration) : structure =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  match mt with
  | { pmtd_type = Some mod_type; pmtd_name; pmtd_loc; _ } -> (
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
          ~name:{ txt = Some "ExprToImpl"; loc }
          ~expr:functor_expr in
      [ { pstr_desc = Pstr_module mod_binding; pstr_loc = loc } ]
    | _ ->
      [ mk_error ~local:mod_type.pmty_loc ~global:loc
          "Expected a module type expression that was a signature"
      ])
  | { pmtd_type = None; pmtd_loc; pmtd_name; _ } ->
    Location.raise_errorf ~loc
      "Can't derive for expressions that aren't module type declarations"

let () =
  (* Generate auxiliary type declarations *)
  let datatype_deriver =
    Deriving.add "mica_types" ~str_module_type_decl:type_generator in
  (* Generate the body of the [ExprToImpl] functor - Note that we must generate
     the declarations of auxiliary datatypes before generating the functor *)
  let functor_generator =
    Deriving.Generator.V2.make_noarg ~deps:[ datatype_deriver ] generate_functor
  in
  Deriving.add "mica" ~str_module_type_decl:functor_generator |> Deriving.ignore
