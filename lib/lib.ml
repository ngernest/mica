open Ppxlib
open Ast_helper
open Ast_builder.Default
open StdLabels
open Utils
open Equality
open Lident
open Miscellany
open Printers
open Getters
open Builders
open Inv_ctx
open Let_open

(******************************************************************************)
(** {1 Generator for Auxiliary Datatypes} *)

(** Walks over all the [val ...] declarations in a module signature
    and creates the corresponding definition of the [expr] ADT 
    - The return type is a list of pairs, where each pair 
    consists of the declaration for the [expr] constructor, 
    along with the return type of the function (expressed as 
    a [core_type]) *)
let mk_expr_cstrs (sig_items : signature) :
  (constructor_declaration * core_type) list =
  List.rev
  @@ List.fold_left sig_items ~init:[] ~f:(fun acc { psig_desc; psig_loc; _ } ->
         match psig_desc with
         | Psig_type (rec_flag, type_decls) -> []
         | Psig_value { pval_name; pval_type; pval_loc; _ } ->
           let name : string = String.capitalize_ascii pval_name.txt in
           (* Exclude the return type of the function from the list of arg types
              for the [expr] constructor *)
           let arg_tys : core_type list =
             remove_last (get_cstr_arg_tys pval_type) in
           (* Return type of the function *)
           let ret_ty = get_ret_ty pval_type in
           (mk_cstr ~name ~loc:pval_loc ~arg_tys, ret_ty) :: acc
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
let mk_val_cstrs (sig_items : signature) : constructor_declaration list =
  mk_cstr_aux sig_items ~f:mk_val_cstr

(** Maps [ty]s to [expr]s (for use in [gen_expr]) 
    - TODO: figure out how to use the result of [destruct_gen_expr_tys]
      when deriving [gen_expr] *)
let destruct_gen_expr_tys (sig_items : signature) :
  (constructor_declaration * constructor_declaration list) list =
  let open Base.List.Assoc in
  let expr_cstrs : (core_type * constructor_declaration) list =
    inverse (mk_expr_cstrs sig_items) in
  let ty_cstrs : constructor_declaration list = mk_ty_cstrs sig_items in
  (* Map [ty] constructors in [ty_cstrs] to the keys in [expr_cstrs], then group
     values with the same keys together *)
  merge_list_with_assoc_list ty_cstrs expr_cstrs ~eq:equal_ty_cstr_core_type
  |> group ~equal:equal_constructor_declaration

(* TODO: - figure out how to do a pattern match on the [ty] constructors inside
   the body of [gen_expr], while keeping track of the [size] QC parameter - ^^
   this will involve doing some analysis of the arities of the functions in the
   signature - when [size = 0], the RHS of the case stmt should be an arity-0
   constructor that is [return]ed into the [Generator] monad - May need to
   create some sort of [inv_ctx]-esque structure to map [ty]s to [expr]s (based
   on the return type of the corresponding function in the signature) *)

(** Derives the [gen_expr] QuickCheck generator 
    - [ty_cstrs] is a list of constructors for the [ty] ADT *)
let derive_gen_expr ~(loc : Location.t)
  (ty_cstrs : constructor_declaration list) : expression =
  (* Derive [let open] expressions for the [Generator.Let_syntax] module *)
  let qc_gen_mod = module_expr_of_string ~loc "Core.Quickcheck.Generator" in
  let let_syntax_mod = module_expr_of_string ~loc "Let_syntax" in
  (* let body = [%expr size >>= fun x -> return x] in *)
  let body =
    [%expr
      let%bind x = small_non_negative_int in
      return x] in
  let_open_twice ~loc qc_gen_mod let_syntax_mod body

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
        (* Type declarations for the [expr] & [ty] ADTs *)
        let expr_td =
          mk_adt ~loc ~name:"expr"
            ~cstrs:(List.map ~f:fst (mk_expr_cstrs sig_items)) in
        let ty_cstrs = mk_ty_cstrs sig_items in
        let ty_td = mk_adt ~loc ~name:"ty" ~cstrs:ty_cstrs in
        [ pstr_type ~loc Recursive [ expr_td ];
          pstr_type ~loc Recursive [ ty_td ];
          [%stri let gen_expr ty = [%e derive_gen_expr ~loc ty_cstrs]]
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
  (Longident.t Location.loc * pattern option * inv_ctx * core_type) list =
  match mod_ty.pmty_desc with
  | Pmty_signature sig_items -> get_cstr_metadata (mk_expr_cstrs sig_items)
  | _ -> failwith "TODO: get_expr_cstrs"

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
    let func_arg_ident = pexp_ident_of_string x ~loc in
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
      let func_args =
        List.map ~f:(fun x -> (Nolabel, pexp_ident_of_string ~loc x)) vars in
      pexp_construct ~loc value_cstr (Some (pexp_apply ~loc mod_item func_args))
    else
      (* Recursively call [interp] on all variables that have type [expr] *)
      let match_arm =
        get_match_arm ~loc:ppat_loc expr_vars ~abs_ty_parameterized in
      let scrutinees = mk_scrutinees expr_vars ~post:(pexp_tuple ~loc) ~loc in
      let func_args =
        List.map
          ~f:(fun x -> pexp_ident_of_string x ~loc)
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
    (contained inside the body of the [TestHarness] functor) 
    - The argument [expr_cstrs] is a list containing the 
    names & arg types of the constructors for the [expr] algebraic data type, 
    along with [inv_ctx], an "inverse typing context" that maps [core_type]'s 
    to variables with that type
    - The optional argument [abs_ty_parameterized] represents whether 
    the abstract type [t] in the module signature is parameterized (e.g. ['a t]) 
    or not *)
let mk_interp ~(loc : location) (mod_ty : module_type)
  ?(abs_ty_parameterized = false)
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
  let arg_ident = pexp_ident ~loc (lident_loc_of_string "e" ~loc) in
  let func_body : expression = pexp_match ~loc arg_ident cases in
  [%stri let rec interp e = [%e func_body]]

(** Creates the body of the [TestHarness] functor *)
let mk_functor ~(loc : location) (arg_name : label option with_loc)
  (mod_ty : module_type) (sig_items : signature)
  (expr_cstrs :
    (Longident.t Location.loc * pattern option * inv_ctx * core_type) list) :
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
  let interp_fun_defn = mk_interp ~loc mod_ty ~abs_ty_parameterized expr_cstrs in
  let functor_body : structure_item list =
    [%str
      [%%i include_decl]
      [%%i val_adt_decl]
      [%%i interp_fun_defn]] in
  let functor_expr : module_expr =
    { pmod_desc = Pmod_structure functor_body;
      pmod_loc = loc;
      pmod_attributes = []
    } in
  pmod_functor ~loc (Named (arg_name, mod_ty)) functor_expr

(** Generates the scaffolding for the [TestHarness] functor 
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
          ~name:{ txt = Some "TestHarness"; loc }
          ~expr:functor_expr in
      [ { pstr_desc = Pstr_module mod_binding; pstr_loc = loc } ]
    | _ ->
      [ mk_error ~local:mod_type.pmty_loc ~global:loc
          "Expected a module type expression that was a signature"
      ])
  | { pmtd_type = None; pmtd_loc; pmtd_name; _ } ->
    Location.raise_errorf ~loc
      "Can't derive for expressions that aren't module type declarations"

(** Labelled arguments for the [mica] PPX deriver 
    TODO: handle the continuation in [generate_functor] *)
let args () =
  Deriving.Args.(empty +> arg "m1" (pexp_ident __) +> arg "m2" (pexp_ident __))

let () =
  List.iter ~f:Reserved_namespaces.reserve [ "mica_types"; "mica" ];
  (* Generate auxiliary type declarations *)
  let datatype_deriver =
    Deriving.add "mica_types" ~str_module_type_decl:type_generator in
  (* Generate the body of the [TestHarness] functor - Note that we must generate
     the declarations of auxiliary datatypes before generating the functor *)
  let functor_generator =
    Deriving.Generator.V2.make_noarg ~deps:[ datatype_deriver ] generate_functor
  in
  Deriving.add "mica" ~str_module_type_decl:functor_generator |> Deriving.ignore
