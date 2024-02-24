open Ppxlib
open Ast_helper
open Ast_builder.Default
open StdLabels
open Utils

(******************************************************************************)
(** {1 Generator for Auxiliary Datatypes} *)

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
    an occurrence of an abstract type in an non-arrow type (e.g. [val empty : 'a t])
    should be ignored (so [val empty : 'a t] corresponds to the 0-arity constructor [Empty]).
*)
let rec get_constructor_arg_tys ?(is_arrow = false) (ty : core_type) :
  core_type list =
  let loc = ty.ptyp_loc in
  match monomorphize ty with
  | ty' when List.mem ty' ~set:(base_types ~loc) -> [ ty' ]
  | { ptyp_desc = Ptyp_constr ({ txt = lident; _ }, _); _ } as ty' ->
    let tyconstr = String.concat ~sep:"" (Astlib.Longident.flatten lident) in
    if String.equal tyconstr !abstract_ty_name then
      if is_arrow then [ [%type: expr] ] else []
    else [ ty' ]
  | { ptyp_desc = Ptyp_arrow (_, t1, t2); _ } ->
    get_constructor_arg_tys ~is_arrow:true t1
    @ get_constructor_arg_tys ~is_arrow:true t2
  | { ptyp_desc = Ptyp_tuple tys; _ } ->
    List.concat_map ~f:(get_constructor_arg_tys ~is_arrow) tys
  | _ -> failwith "TODO: get_constructor_arg_tys"

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

(** Walks over all the [val ...] declarations in a module signature
    and creates the corresponding definition of the [expr] ADT *)
let mk_expr_constructors (sig_items : signature) : constructor_declaration list
    =
  List.fold_left sig_items ~init:[] ~f:(fun acc { psig_desc; psig_loc; _ } ->
    match psig_desc with
    | Psig_type (rec_flag, type_decls) -> []
    | Psig_value { pval_name; pval_type; pval_loc; _ } ->
      let name = String.capitalize_ascii pval_name.txt in
      (* Exclude the return type of the function from the list of argument types
         for the [expr] data constructor *)
      let arg_tys = remove_last (get_constructor_arg_tys pval_type) in
      mk_constructor ~name ~loc:pval_loc ~arg_tys :: acc
    | Psig_attribute attr -> failwith "TODO: handle attribute [@@@id]"
    | Psig_extension (ext, attrs) -> failwith "TODO: handle extensions"
    | _ ->
      failwith
        "TODO: not sure how to handle other kinds of [signature_item_desc]")

(** Extracts the unique return types of all [val] declarations within a 
    module signature *)
let uniq_ret_tys (sig_items : signature) : core_type list =
  List.fold_left sig_items ~init:[] ~f:(fun acc { psig_desc; psig_loc; _ } ->
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
let mk_constructor_aux (sig_items : signature)
  ~(f : core_type -> constructor_declaration) : constructor_declaration list =
  let ret_tys = uniq_ret_tys sig_items in
  let uniq_ret_tys =
    List.sort_uniq ret_tys ~cmp:(fun t1 t2 ->
      String.compare (string_of_core_ty t1) (string_of_core_ty t2))
  in
  List.map uniq_ret_tys ~f

(** Constructs the definition of the [ty] algebraic data type
    based on the unique return types of all [val] declarations within 
    the module signature *)
let mk_ty_constructors (sig_items : signature) : constructor_declaration list =
  mk_constructor_aux sig_items ~f:(fun ty ->
    mk_constructor ~name:(string_of_core_ty ty) ~loc:ty.ptyp_loc ~arg_tys:[])

(** Constructs the definition of the [value] algebraic data type
    based on the inhabitants of the [ty] ADT *)
let mk_val_constructors (sig_items : signature) =
  mk_constructor_aux sig_items ~f:(fun ty ->
    mk_constructor
      ~name:("Val" ^ string_of_core_ty ty)
      ~loc:ty.ptyp_loc ~arg_tys:[ ty ])

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
        [
          mk_error ~local:pmtd_loc ~global:loc "Module signature can't be empty";
        ]
      | _ ->
        let expr_td =
          mk_adt ~loc ~name:"expr"
            ~constructors:(List.rev (mk_expr_constructors sig_items))
        in
        let ty_td =
          mk_adt ~loc ~name:"ty" ~constructors:(mk_ty_constructors sig_items)
        in
        [
          pstr_type ~loc Recursive [ expr_td ];
          pstr_type ~loc Recursive [ ty_td ];
        ])
    | _ -> failwith "TODO: other case for mod_type")
  | { pmtd_type = None; pmtd_loc; pmtd_name; _ } ->
    [
      mk_error ~local:pmtd_loc ~global:loc
        "Can't derive for expressions that aren't module type declarations";
    ]

(** Instantiates the PPX deriver for [expr]s *)
let type_generator :
  (structure_item list, module_type_declaration) Deriving.Generator.t =
  Deriving.Generator.V2.make_noarg generate_types_from_sig

(******************************************************************************)

(** {1 Generator for Functors} *)

(** Helper function: given [mod_ty], a module signature,
    [get_expr_constructors] produces [expr] constructor names & arguments
    that match the declarations in the module signature *)
let get_expr_constructors (mod_ty : module_type) :
  (Longident.t Location.loc * pattern) list =
  match mod_ty.pmty_desc with
  | Pmty_signature sig_items ->
    get_constructor_names @@ mk_expr_constructors sig_items
  | _ -> failwith "TODO: get_expr_constructors"

(** Creates the definition for the [interp] function 
    (contained inside the body of the [ExprToImpl] functor) 
    - The argument [expr_cstrs] is a list containing the 
    names & arg types of the constructors for the [expr] algebraic data type *)
let mk_interp ~(loc : location) (mod_ty : module_type)
  (expr_cstrs : (Longident.t Location.loc * pattern) list) : structure_item =
  (* String literal denoting the argument name *)
  let arg_str = "e" in
  let arg_ident : expression =
    pexp_ident ~loc (with_loc (Lident arg_str) ~loc)
  in
  let func_name_pat : pattern = ppat_var ~loc { txt = "interp"; loc } in
  let func_arg : pattern = ppat_var ~loc { txt = arg_str; loc } in
  (* TODO: update placeholder RHS of pattern match *)
  let placeholder_rhs = pexp_constant ~loc (Pconst_integer ("1", None)) in
  (* Each [expr] constructor corresponds to the LHS of a pattern match case *)
  let patterns : pattern list =
    List.map expr_cstrs ~f:(fun (cstr, args) ->
      ppat_construct ~loc cstr (Some args))
  in
  let wildcard : pattern = ppat_any ~loc in
  (* TODO: figure out how to generate fresh arguments for the [expr]
     constructors that match their arity *)
  let func_body : expression =
    [%expr
      match [%e arg_ident] with
      | [%p List.hd patterns] -> [%e placeholder_rhs]
      | [%p List.hd (List.tl patterns)] -> [%e placeholder_rhs]
      | [%p wildcard] -> [%e placeholder_rhs]]
  in
  let func_binding : expression =
    pexp_fun ~loc Nolabel None func_arg func_body
  in
  let func_defn : value_binding =
    value_binding ~loc ~pat:func_name_pat ~expr:func_binding
  in
  pstr_value ~loc Recursive [ func_defn ]

(** Creates the body of the [ExprToImpl] functor *)
let mk_functor ~(loc : location) (arg_name : label option with_loc)
  (mod_ty : module_type) (sig_items : signature)
  (expr_cstrs : (Longident.t Location.loc * pattern) list) : module_expr =
  (* [include M] declaration *)
  let m_ident =
    { txt = Longident.parse (Option.value arg_name.txt ~default:"M"); loc }
  in
  let m_expr = pmod_ident ~loc m_ident in
  let include_decl = pstr_include ~loc (include_infos ~loc m_expr) in

  (* Declaration for the [value] ADT *)
  let val_adt =
    mk_adt ~loc ~name:"value" ~constructors:(mk_val_constructors sig_items)
  in
  let val_adt_decl = pstr_type ~loc Recursive [ val_adt ] in

  (* Assembling all the components of the functor *)
  let functor_body =
    [ include_decl; val_adt_decl; mk_interp ~loc mod_ty expr_cstrs ]
  in
  let functor_expr =
    {
      pmod_desc = Pmod_structure functor_body;
      pmod_loc = loc;
      pmod_attributes = [];
    }
  in
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
      let expr_cstrs = get_expr_constructors mod_type in
      let functor_expr =
        mk_functor ~loc new_name mod_type_alias sig_items expr_cstrs
      in
      let mod_binding =
        module_binding ~loc
          ~name:{ txt = Some "ExprToImpl"; loc }
          ~expr:functor_expr
      in
      [ { pstr_desc = Pstr_module mod_binding; pstr_loc = loc } ]
    | _ ->
      [
        mk_error ~local:mod_type.pmty_loc ~global:loc
          "Expected a module type expression that was a signature";
      ])
  | { pmtd_type = None; pmtd_loc; pmtd_name; _ } ->
    Location.raise_errorf ~loc
      "Can't derive for expressions that aren't module type declarations"

let () =
  (* Generate auxiliary type declarations *)
  let datatype_deriver =
    Deriving.add "mica_types" ~str_module_type_decl:type_generator
  in
  (* Generate the body of the [ExprToImpl] functor - Note that we must generate
     the declarations of auxiliary datatypes before generating the functor *)
  let functor_generator =
    Deriving.Generator.V2.make_noarg ~deps:[ datatype_deriver ] generate_functor
  in
  Deriving.add "mica" ~str_module_type_decl:functor_generator |> Deriving.ignore
