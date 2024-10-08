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
  List.fold_left sig_items ~init:[] ~f:(fun acc { psig_desc; _ } ->
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
  List.fold_left sig_items ~init:[] ~f:(fun acc { psig_desc; _ } ->
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

(** Takes the name of a type and produces the name of its 
    corresponding QuickCheck generator *)
let mk_generator_name (s : string) : string =
  Printf.sprintf "quickcheck_generator_%s" s

(** Produces an atomic QuickCheck generator for the given [core_type] 
    - [abs_tys] is an association list consisting of type names & type parameters
    for the abstract types in the signature *)
let rec gen_atom ~(loc : Location.t) (ty : core_type)
  ~(abs_tys : (string * core_type list) list) : expression =
  match ty.ptyp_desc with
  | Ptyp_constr (ty_name, []) -> (
    (* Check whether the type is an abstract type in the signature *)
    let ty_str =
      (* Convert [expr] to [t] by default (TODO: may want to find a better
         representation in the future) *)
      if String.equal (string_of_lident ty_name.txt) "expr" then "t"
      else string_of_lident ty_name.txt in
    match List.assoc_opt ty_str abs_tys with
    | Some [] ->
      (* If the abstract type has no type params, produce a recursive call with
         [T] *)
      let ty_expr = evar ~loc "T" in
      [%expr with_size ~size:(k / 2) (gen_expr [%e ty_expr])]
    | Some tyvars ->
      (* Monomorphize all type variables, obtain their string representation &
         concat the resultant string, so [('a, 'b) t] becomes [IntIntT] *)
      let tyvars_prefix =
        String.concat ~sep:"" (List.map ~f:string_of_monomorphized_ty tyvars)
      in
      let ty_expr = evar ~loc (tyvars_prefix ^ "T") in
      [%expr with_size ~size:(k / 2) (gen_expr [%e ty_expr])]
    | None ->
      (* Assume [quickcheck_generator_ty] exists for any other non-parameterized
         type [ty] *)
      unapplied_type_constr_conv ~loc ty_name ~f:mk_generator_name)
  (* For parameterized types, recursively derive generators for their type
     parameters *)
  | Ptyp_constr (ty_name, ty_params) ->
    let args = List.map ~f:(gen_atom ~loc ~abs_tys) ty_params in
    type_constr_conv ~loc ty_name ~f:mk_generator_name args
  | Ptyp_any ->
    mk_error_expr ~loc:ty.ptyp_loc
      "types must be instantiated in order to derive a QuickCheck generator"
  | Ptyp_var _ ->
    (* Instantiate type variables with [int] *)
    [%expr quickcheck_generator_int]
  | Ptyp_tuple tys ->
    (* [Core.Quickcheck.Generator] only supports tuples of length 2 - 6 *)
    let n = List.length tys in
    if n >= 2 && n <= 6 then
      let tuple_gen = evar ~loc @@ Printf.sprintf "tuple%d" n in
      let args = List.map ~f:(gen_atom ~loc ~abs_tys) tys in
      eapply ~loc tuple_gen args
    else
      pexp_extension ~loc
      @@ Location.error_extensionf ~loc
           "Unable to derive generator for product type %s with %d types"
           (Ppxlib.string_of_core_type ty)
           n
  (* TODO: derive QC generator for unary functions of type [int -> int] *)
  | Ptyp_arrow (Nolabel, _, _) ->
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

(** Produces the name of QuickCheck generators corresponding to a list of 
    [constructor_declaration]s (by prepending the prefix "gen" to each 
    constructor's name) *)
let mint_generator_names (cstrs : constructor_declaration list) : string list =
  List.map
    ~f:(fun cstr ->
      let cstr_name = cstr.pcd_name.txt in
      Expansion_helpers.mangle (Prefix "gen") (uncapitalize cstr_name))
    cstrs

(** Helper function for producing the RHS of the pattern match in gen_expr
    - [abs_tys] is an association list consisting of type names & type parameters
    for the abstract types in the signature  *)
let gen_expr_rhs ~(loc : Location.t) (cstrs : constructor_declaration list)
  ~(abs_tys : (string * core_type list) list) : expression =
  match cstrs with
  | [] -> failwith "impossible"
  | _ ->
    let generator_names = mint_generator_names cstrs in
    List.map2 cstrs generator_names ~f:(fun cstr cstr_gen_name ->
        let cstr_name_evar = evar ~loc cstr.pcd_name.txt in
        let cstr_arg_tys = get_cstr_arg_tys cstr in

        (* The name of the generator for the symbolic expression, e.g.
           [gen_Empty] *)
        let cstr_gen_name_var = pvar ~loc cstr_gen_name in

        (* Generators for nullary constructors are trivial, i.e. they just
           [return] the constructor *)
        if get_cstr_arity cstr = 0 then
          value_binding ~loc ~pat:cstr_gen_name_var
            ~expr:[%expr return [%e cstr_name_evar]]
        else
          (* Fresh names for the generators of the constructor arguments *)
          let arg_gen_names : string list =
            List.map ~f:(fun _ -> gen_symbol ~prefix:"g" ()) cstr_arg_tys in
          (* [let g1 = gen_int and g2 = gen_string in ...] *)
          let atomic_generators : value_binding list =
            List.map2
              ~f:(fun ty arg_gen ->
                let pat = pvar ~loc arg_gen in
                let gen_body = gen_atom ~loc ty ~abs_tys in
                value_binding ~loc ~pat ~expr:gen_body)
              cstr_arg_tys arg_gen_names in
          let gen_cstr_let_body =
            match arg_gen_names with
            | [] -> failwith "impossible, arity must be > 0"
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
                let vars = List.map ~f:(fun _ -> gen_symbol ~prefix:"e" ()) gs in
                let args_pat = ppat_tuple ~loc (List.map ~f:(pvar ~loc) vars) in
                let args_expr = pexp_tuple ~loc (List.map ~f:(evar ~loc) vars) in
                [%expr
                  [%e eapply ~loc tuple_gen generators] >>| fun [%p args_pat] ->
                  [%e cstr_name_evar] [%e args_expr]]
              else
                pexp_extension ~loc
                @@ Location.error_extensionf ~loc
                     "Functions with arity %d not supported, max arity is 6\n" n
          in
          (* [ let g1 = ... and g2 = ... in ... ] *)
          let gen_cstr_let_expr =
            pexp_let ~loc Nonrecursive atomic_generators gen_cstr_let_body in
          (* [let gen_is_empty = ... ] *)
          value_binding ~loc ~pat:cstr_gen_name_var ~expr:gen_cstr_let_expr)
    |> fun val_bindings ->
    let gen_name_evars = elist ~loc (List.map ~f:(evar ~loc) generator_names) in
    pexp_let ~loc Nonrecursive val_bindings [%expr union [%e gen_name_evars]]

(** Determines if an [expr] constructor can be used as a base case for 
    [gen_expr]. A constructor can be used as the base case if:
    - It is nullary
    - It has no arguments of type [expr] (i.e. the corresponding function
      in the signature has no arguments of type [t]) 
    - Note: constructors with record arguments are currently unsupported. *)
let is_base_case (cstr : constructor_declaration) : bool =
  let loc = cstr.pcd_loc in
  match cstr.pcd_args with
  | Pcstr_tuple [] -> true
  | Pcstr_tuple xs ->
    (* Check whether there are any arguments of type [expr] *)
    list_is_empty (List.filter ~f:(equal_core_type [%type: expr]) xs)
  | Pcstr_record _ -> false

(** [check_type_is_concrete abs_ty_names ty] determines whether [ty] is a 
    {i concrete type} based on [abs_ty_names], a list containing the names of 
    abstract types defined in a signature. 
    
    For example, if a module signature defines an abstract type ['a t], 
    then [int t] would {i not} be concrete, but [int] and [bool] would be 
    considered concrete. 
    - Note: type variables (e.g. ['a]) are considered concrete by this function
    (since they're technically not defined inside a module signature) *)
let rec check_type_is_concrete (abs_ty_names : string list) (ty : core_type) :
  bool =
  match ty.ptyp_desc with
  (* For arrow & product types, check if all constituent types are concrete via
     structural recursion *)
  | Ptyp_arrow (_, t1, t2) ->
    check_type_is_concrete abs_ty_names t1
    && check_type_is_concrete abs_ty_names t2
  | Ptyp_tuple tys ->
    List.fold_left
      ~f:(fun acc t -> acc && check_type_is_concrete abs_ty_names t)
      ~init:true tys
  (* For base types (nullary type constructors), check if the name of the type
     constructor is contained in the list [abs_ty_names] *)
  | Ptyp_constr ({ txt = tyconstr; _ }, []) ->
    let tyconstr_name = string_of_lident tyconstr in
    not (List.mem tyconstr_name ~set:abs_ty_names)
  (* Do the same for parametreized types, but in addition, also check that all
     the type parameters are concrete *)
  | Ptyp_constr ({ txt = tyconstr; _ }, arg_tys) ->
    let tyconstr_name = string_of_lident tyconstr in
    (not (List.mem tyconstr_name ~set:abs_ty_names))
    && List.fold_left
         ~f:(fun acc arg_ty ->
           acc && check_type_is_concrete abs_ty_names arg_ty)
         ~init:true arg_tys
  | _ -> true

(** [mk_gen_expr_case abs_tys ~is_base_case ty rhs_cstrs] constructs a single case in the 
    pattern-match of the body of [gen_expr].
    - [abs_tys] is a list containing pairs of the form
    [(<type_name>, <list_of_type_parameters>)]. Most likely, this list is 
    obtained by calling [get_ty_decls_from_sig] in [getters.ml]. 
    - [ty] is the type we are matching on in the LHS of the pattern match 
    inside [gen_expr]
    - [rhs_cstrs] are the constructors for [expr] that have that type (to be 
    generated on the RHS of the pattern match). 
    - [is_base_case] is an optional Boolean argument that indicates whether 
    the constructors in [rhs_cstrs] are base cases for [gen_expr] 
    (as determined by the [is_base_case] function). This parameter 
    defaults to [false]. *)
let mk_gen_expr_case (abs_tys : (string * core_type list) list)
  ?(is_base_case = false) (ty : core_type)
  (rhs_cstrs : constructor_declaration list) : case =
  let loc = ty.ptyp_loc in
  (* When matching on base cases, we want [gen_expr]'s QuickCheck size parameter
     to be 0. For non-trivial cases, any non-zero size is fine. *)
  let size_pat = if is_base_case then [%pat? 0] else [%pat? _] in
  (* The LHS of the pattern match is a pair fo the form [(ty,
     quickcheck_size]) *)
  let lhs =
    ppat_tuple ~loc [ pvar ~loc (string_of_monomorphized_ty ty); size_pat ]
  in
  let rhs_exprs = gen_expr_rhs ~loc:lhs.ppat_loc ~abs_tys rhs_cstrs in
  let rhs = [%expr [%e rhs_exprs]] in
  case ~lhs ~guard:None ~rhs

(** Creates the main case statement in [gen_expr] *)
let gen_expr_cases (sig_items : signature) : case list =
  let open Base.List.Assoc in
  let abs_tys = get_ty_decls_from_sig sig_items in
  let abs_ty_names = List.map ~f:fst abs_tys in
  (* Maps [ty]s to [expr]s *)
  let skeleton : (core_type * constructor_declaration list) list =
    inverse (mk_expr_cstrs sig_items)
    |> sort_and_group ~compare:compare_core_type in
  List.concat_map skeleton ~f:(fun (ty, rhs_cstrs) ->
      if not (check_type_is_concrete abs_ty_names ty) then
        let base_case_cstrs, non_trivial_cstrs =
          List.partition ~f:is_base_case rhs_cstrs in
        let base_case =
          mk_gen_expr_case abs_tys ty base_case_cstrs ~is_base_case:true in
        let non_trivial_cases =
          [ mk_gen_expr_case abs_tys ty non_trivial_cstrs ~is_base_case:false ]
        in
        base_case :: non_trivial_cases
      else [ mk_gen_expr_case abs_tys ty rhs_cstrs ~is_base_case:false ])

(** Derives the [gen_expr] QuickCheck generator *)
let derive_gen_expr ~(loc : Location.t) (sig_items : signature) : expression =
  (* Derive [let open] expressions for the necessary modules *)
  let core_mod = module_expr_of_string ~loc "Core" in
  let qc_gen_mod = module_expr_of_string ~loc "Quickcheck.Generator" in
  let let_syntax_mod = module_expr_of_string ~loc "Let_syntax" in
  let match_exp = pexp_match ~loc [%expr ty, k] (gen_expr_cases sig_items) in
  let body = [%expr size >>= fun k -> [%e match_exp]] in
  let_open ~loc core_mod (let_open_twice ~loc qc_gen_mod let_syntax_mod body)

(** Produces the attribute [[@@deriving show { with_path = false }]] *)
let deriving_show ~(loc : Location.t) : attribute =
  deriving_attribute ~loc [%expr show { with_path = false }]

(** Walks over a module signature definition and extracts the 
    abstract type declaration, producing the definition 
    the [expr] and [ty] algebraic data types *)
let generate_types_from_sig ~(ctxt : Expansion_context.Deriver.t)
  (mt : module_type_declaration) : structure_item list =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  match mt with
  | { pmtd_type = Some mod_type; pmtd_loc; _ } -> (
    match mod_type with
    | { pmty_desc = Pmty_signature sig_items; _ } -> (
      match sig_items with
      | [] ->
        [ mk_error_pstr ~local:pmtd_loc ~global:loc "Module sig can't be empty"
        ]
      | _ ->
        (* Type declarations for the [expr] & [ty] ADTs *)
        let expr_td =
          mk_adt ~loc ~name:"expr"
            ~cstrs:(List.map ~f:fst (mk_expr_cstrs sig_items)) in
        (* Attach [[@@deriving show]] to the type definition *)
        let annotated_expr_td =
          { expr_td with ptype_attributes = [ deriving_show ~loc ] } in
        let ty_cstrs = mk_ty_cstrs sig_items in
        let ty_td = mk_adt ~loc ~name:"ty" ~cstrs:ty_cstrs in
        let annotated_ty_td =
          { ty_td with ptype_attributes = [ deriving_show ~loc ] } in
        [%str
          [%%i pstr_type ~loc Recursive [ annotated_expr_td ]]
          [%%i pstr_type ~loc Recursive [ annotated_ty_td ]]

          let rec gen_expr ty = [%e derive_gen_expr ~loc sig_items]])
    | _ -> failwith "TODO: other case for mod_type")
  | { pmtd_type = None; pmtd_loc; _ } ->
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
