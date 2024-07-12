open Ppxlib
open Ast_builder.Default
open StdLabels
open Utils

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
  let abs_ty_names = get_abs_ty_names sig_items in
  List.fold_left sig_items ~init:[] ~f:(fun acc { psig_desc; psig_loc; _ } ->
      match psig_desc with
      | Psig_type (_, _) -> []
      | Psig_value { pval_name; pval_type; pval_loc; _ } ->
        let name : string = String.capitalize_ascii pval_name.txt in
        (* Exclude the return type of the function from the list of arg types
           for the [expr] constructor *)
        let arg_tys =
          remove_last (get_arg_tys_of_expr_cstr pval_type abs_ty_names) in
        (* Return type of the function *)
        let ret_ty = get_ret_ty pval_type in
        (mk_cstr ~name ~loc:pval_loc ~arg_tys, ret_ty) :: acc
      | Psig_attribute _ -> failwith "TODO: handle attribute [@@@id]"
      | Psig_extension (_, _) -> failwith "TODO: handle extensions"
      | _ ->
        failwith
          "TODO: not sure how to handle other kinds of [signature_item_desc]")
  |> List.rev

(** Extracts the unique return types of all [val] declarations within a 
      module signature *)
let uniq_ret_tys (sig_items : signature) : core_type list =
  List.fold_left sig_items ~init:[] ~f:(fun acc { psig_desc; psig_loc; _ } ->
      match psig_desc with
      | Psig_value { pval_type; _ } ->
        let ty = get_ret_ty pval_type in
        if List.mem ty ~set:acc then acc else ty :: acc
      | _ -> acc)
  |> List.rev

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
        String.compare
          (string_of_monomorphized_ty t1)
          (string_of_monomorphized_ty t2)) in
  List.map uniq_ret_tys ~f

(** Constructs the definition of the [ty] algebraic data type
    based on the unique return types of all [val] declarations within 
    the module signature *)
let mk_ty_cstrs (sig_items : signature) : constructor_declaration list =
  mk_cstr_aux sig_items ~f:(fun ty ->
      mk_cstr ~name:(string_of_monomorphized_ty ty) ~loc:ty.ptyp_loc ~arg_tys:[])

(** [mk_val_cstr ty] constructors the corresponding constructor declaration
    for the [value] datatype, given some [core_type] [ty]
    - e.g. if [ty = Int], [mk_val_cstr] returns the declaration for 
      the [ValInt] constructor *)
let mk_val_cstr (ty : core_type) : constructor_declaration =
  mk_cstr
    ~name:("Val" ^ string_of_monomorphized_ty ty)
    ~loc:ty.ptyp_loc ~arg_tys:[ ty ]

(** Constructs the definition of the [value] algebraic data type
      based on the inhabitants of the [ty] ADT *)
let mk_val_cstrs (sig_items : signature) : constructor_declaration list =
  mk_cstr_aux sig_items ~f:mk_val_cstr

type spine = { cstr : constructor_declaration; args : expression list }

let mk_spine (cstr : constructor_declaration) (args : expression list) =
  { cstr; args }

(** Takes the name of a type and produces the name of its 
    corresponding QuickCheck generator *)
let mk_generator_name (s : string) : string =
  Printf.sprintf "quickcheck_generator_%s" s

(** Produces an atomic QuickCheck generator for the given [core_type] *)
let rec gen_atom ~(loc : Location.t) (ty : core_type) : expression =
  match ty.ptyp_desc with
  | Ptyp_constr (ty_name, []) ->
    (* For type [expr], produce a recursive call to [gen_expr] *)
    if equal_longident ty_name.txt (Longident.parse "expr") then
      [%expr with_size ~size:(k / 2) (gen_expr T)]
    else
      (* Base case: assume that [quickcheck_generator_ty] exists for any other
         non-parameterized type [ty] *)
      unapplied_type_constr_conv ~loc ty_name ~f:mk_generator_name
  (* For parameterized types, recursively derive generators for their type
     parameters *)
  | Ptyp_constr (ty_name, ty_params) ->
    let args = List.map ~f:(gen_atom ~loc) ty_params in
    type_constr_conv ~loc ty_name ~f:mk_generator_name args
  | Ptyp_any ->
    mk_error_expr ~loc:ty.ptyp_loc
      "types must be instantiated in order to derive a QuickCheck generator"
  | Ptyp_var tyvar ->
    (* Instantiate type variables with [int] *)
    [%expr quickcheck_generator_int]
  | Ptyp_tuple tys ->
    (* [Core.Quickcheck.Generator] only supports tuples of length 2 - 6 *)
    let n = List.length tys in
    if n >= 2 && n <= 6 then
      let tuple_gen = evar ~loc @@ Printf.sprintf "tuple%d" n in
      let args = List.map ~f:(gen_atom ~loc) tys in
      eapply ~loc tuple_gen args
    else
      pexp_extension ~loc
      @@ Location.error_extensionf ~loc
           "Unable to derive QuickCheck generator for product type %s \
            containing %d types"
           (Ppxlib.string_of_core_type ty)
           n
  (* TODO: derive QC generator for unary functions *)
  | Ptyp_arrow (Nolabel, arg_ty, ret_ty) ->
    pexp_extension ~loc
    @@ Location.error_extensionf ~loc
         "Function types not supported yet (to be implemented)"
  | Ptyp_arrow (Labelled lbl, _, _) | Ptyp_arrow (Optional lbl, _, _) ->
    pexp_extension ~loc
    @@ Location.error_extensionf ~loc
         "Unable to derive QuickCheck generator for function type %s with \
          labelled/optional argument %s"
         (Ppxlib.string_of_core_type ty)
         lbl
  | _ ->
    pexp_extension ~loc
    @@ Location.error_extensionf ~loc
         "Unable to derive QuickCheck generator for type %s"
         (Ppxlib.string_of_core_type ty)

(* temp function for producing the RHS of the pattern match in gen_expr *)
let rhs ~(loc : Location.t) ty { cstr; args } =
  let cstr_name = cstr.pcd_name.txt in
  let cstr_name_evar = evar ~loc cstr_name in
  let gen_cstr_name =
    Expansion_helpers.mangle (Prefix "gen") (uncapitalize cstr_name) in
  let gen_cstr_expr = evar ~loc gen_cstr_name in
  let gen_cstr_pat = pvar ~loc gen_cstr_name in
  let cstr_arg_tys = get_cstr_arg_tys cstr in
  let generator_names : string list =
    List.map ~f:(fun ty -> gen_symbol ~prefix:"g" ()) cstr_arg_tys in
  (* [let g1 = gen_int and g2 = gen_string in ...] *)
  let atomic_generators : value_binding list =
    List.map2
      ~f:(fun ty gen_name ->
        let pat = pvar ~loc gen_name in
        let gen_body = gen_atom ~loc ty in
        value_binding ~loc ~pat ~expr:gen_body)
      cstr_arg_tys generator_names in
  let gen_cstr_let_body =
    match generator_names with
    | [] ->
      (* TODO: figure out how to do nullary case *)
      [%expr return [%e cstr_name_evar]]
    | [ g ] ->
      (* Generate a fresh name for the random [expr] *)
      let var = gen_symbol ~prefix:"e" () in
      (* Then, apply the constructor to the random [expr] *)
      [%expr
        [%e evar ~loc g] >>| fun [%p pvar ~loc var] ->
        [%e cstr_name_evar] [%e evar ~loc var]]
    | gs ->
      let n = List.length gs in
      if n >= 2 && n <= 6 then
        let tuple_gen = evar ~loc @@ Printf.sprintf "tuple%d" n in
        let generators = List.map ~f:(evar ~loc) gs in
        [%expr
          [%e eapply ~loc tuple_gen generators] >>| fun _ ->
          return
            "TODO: need to produce the call to >>| and invoke the generator \
             here!"]
      else
        pexp_extension ~loc
        @@ Location.error_extensionf ~loc
             "Functions with arity %d not supported, max arity is 6\n" n in

  let gen_cstr_let_expr =
    pexp_let ~loc Nonrecursive atomic_generators gen_cstr_let_body in

  pexp_let Nonrecursive ~loc
    [ value_binding ~loc ~pat:gen_cstr_pat ~expr:gen_cstr_let_expr ]
    gen_cstr_expr

(* TODO: figure out how to invoke [gen_atom] defined above - we want both the
   LHS & RHS to be [core_type]s - we also need to recursively produce monadic
   bind expressions using >>= *)
let gen_expr_cases (sig_items : signature) : case list =
  let open Base.List.Assoc in
  (* Maps [ty]s to [expr]s *)
  let skeleton : (core_type * spine list) list =
    inverse (mk_expr_cstrs sig_items)
    |> List.map ~f:(fun (ty, expr_cstr) ->
           ( ty,
             mk_spine expr_cstr
               (evars_of_cstr_args ~loc:expr_cstr.pcd_loc expr_cstr.pcd_args) ))
    |> group ~equal:equal_core_type in
  List.map skeleton ~f:(fun (ty, rhs_elts) ->
      let lhs = pvar ~loc:ty.ptyp_loc (string_of_monomorphized_ty ty) in
      let loc = lhs.ppat_loc in
      let rhs_exprs =
        elist ~loc (List.map ~f:(fun cstr -> rhs ~loc ty cstr) rhs_elts) in
      let loc = rhs_exprs.pexp_loc in
      let rhs = [%expr [%e rhs_exprs]] in
      case ~lhs ~guard:None ~rhs)

(** Derives the [gen_expr] QuickCheck generator 
    - [ty_cstrs] is a list of constructors for the [ty] ADT *)
let derive_gen_expr ~(loc : Location.t)
  (ty_cstrs : constructor_declaration list) (sig_items : signature) : expression
    =
  (* Derive [let open] expressions for the necessary modules *)
  let core_mod = module_expr_of_string ~loc "Core" in
  let qc_gen_mod = module_expr_of_string ~loc "Quickcheck.Generator" in
  let let_syntax_mod = module_expr_of_string ~loc "Let_syntax" in
  (* let body = [%expr size >>= fun x -> return x] in *)
  let match_exp = pexp_match ~loc [%expr ty] (gen_expr_cases sig_items) in
  let body = [%expr size >>= fun k -> [%e match_exp]] in
  let_open ~loc core_mod (let_open_twice ~loc qc_gen_mod let_syntax_mod body)

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
        [ mk_error_pstr ~local:pmtd_loc ~global:loc "Module sig can't be empty"
        ]
      | _ ->
        (* Type declarations for the [expr] & [ty] ADTs *)
        let expr_td =
          mk_adt ~loc ~name:"expr"
            ~cstrs:(List.map ~f:fst (mk_expr_cstrs sig_items)) in
        let ty_cstrs = mk_ty_cstrs sig_items in
        let ty_td = mk_adt ~loc ~name:"ty" ~cstrs:ty_cstrs in
        [ pstr_type ~loc Recursive [ expr_td ];
          pstr_type ~loc Recursive [ ty_td ];
          [%stri
            let rec gen_expr ty = [%e derive_gen_expr ~loc ty_cstrs sig_items]]
        ])
    | _ -> failwith "TODO: other case for mod_type")
  | { pmtd_type = None; pmtd_loc; pmtd_name; _ } ->
    [ mk_error_pstr ~local:pmtd_loc ~global:loc
        "Can't derive for expressions that aren't module type declarations"
    ]

(** Helper function: given [mod_ty], a module signature,
      [get_expr_cstrs] produces [expr] constructor names & arguments
      that match the declarations in the module signature *)
let get_expr_cstrs (mod_ty : module_type) :
  (Longident.t Location.loc * pattern option * inv_ctx * core_type) list =
  match mod_ty.pmty_desc with
  | Pmty_signature sig_items -> get_cstr_metadata (mk_expr_cstrs sig_items)
  | _ -> failwith "TODO: get_expr_cstrs"
