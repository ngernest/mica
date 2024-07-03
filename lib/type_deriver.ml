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
        let arg_tys = remove_last (get_cstr_arg_tys pval_type abs_ty_names) in
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
      - TODO: figure out how to use the result of [gen_expr_case_skeleton]
        when deriving [gen_expr]
      - TODO: figure out how to create values of type [case] *)
let gen_expr_case_skeleton (sig_items : signature) :
  (Longident.t Location.loc * Longident.t Location.loc list) list =
  let open Base.List.Assoc in
  (* TODOs:
     - use Metaquot instead? 
     - write tests for [gen_expr_case_skeleton] to inspect the AssocList
     - find some way of using [get_cstr_arity] *)
  let expr_cstrs =
    inverse (mk_expr_cstrs sig_items)
    |> List.map ~f:(fun (ty, cstr_decl) ->
           ( lident_loc_of_string ~loc:ty.ptyp_loc (string_of_core_ty ty),
             get_cstr_name cstr_decl ))
    |> List.sort ~cmp:(fun (t1, _) (t2, _) -> Longident.compare t1.txt t2.txt) in
  let ty_cstrs : Longident.t Location.loc list =
    List.map ~f:get_cstr_name (mk_ty_cstrs sig_items) in
  (* Map [ty] constructors in [ty_cstrs] to the keys in [expr_cstrs], then group
     values with the same keys together *)
  merge_list_with_assoc_list ty_cstrs expr_cstrs ~eq:equal_longident_loc
  |> group ~equal:equal_longident_loc

let gen_expr_cases (sig_items : signature) : case list =
  let skeleton = gen_expr_case_skeleton sig_items in
  let guard = None in
  List.map skeleton ~f:(fun (lhs_cstr, rhs_cases) ->
      let lhs = ppat_construct ~loc:lhs_cstr.loc lhs_cstr None in
      let rhs_head = List.hd rhs_cases in 
      let rhs_loc = rhs_head.loc in 
      let rhs_args = pexp_list ~loc:rhs_loc (List.map 
        ~f:(fun rhs_elt -> pexp_ident ~loc:rhs_elt.loc rhs_elt) rhs_cases) in 
      let loc = rhs_args.pexp_loc in 
      let rhs = [%expr of_list [%e rhs_args]] in 
      case ~lhs ~guard ~rhs)
      
      
      (* TODO: figure out how to genreate arguments for the RHS constructors -
         rewrite! *)
      (* let rhs =
        pexp_tuple ~loc:Location.none
          (List.map
             ~f:(fun rhs -> pexp_construct ~loc:rhs.loc rhs None)
             rhs_cases) in
      case ~lhs ~guard ~rhs) *)

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
  (ty_cstrs : constructor_declaration list) (sig_items : signature) : expression
    =
  (* Derive [let open] expressions for the [Generator.Let_syntax] module *)
  let qc_gen_mod = module_expr_of_string ~loc "Core.Quickcheck.Generator" in
  let let_syntax_mod = module_expr_of_string ~loc "Let_syntax" in
  (* let body = [%expr size >>= fun x -> return x] in *)
  let match_exp = pexp_match ~loc [%expr ty] (gen_expr_cases sig_items) in 
  let body = [%expr size >>= fun x -> [%e match_exp]] in 
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
          [%stri let gen_expr ty = [%e derive_gen_expr ~loc ty_cstrs sig_items]]
        ])
    | _ -> failwith "TODO: other case for mod_type")
  | { pmtd_type = None; pmtd_loc; pmtd_name; _ } ->
    [ mk_error ~local:pmtd_loc ~global:loc
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
