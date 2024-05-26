open Ppxlib
open Ast_helper
open Ast_builder.Default
open StdLabels

(* Suppress "unused value" compiler warnings *)
[@@@ocaml.warning "-26-27-32-33-34"]

(*******************************************************************************)
(** {1 Miscellany} *)

let printf = Stdio.printf

(** Constructs a [loc] given some payload [txt] and a location [loc] *)
let with_loc (txt : 'a) ~(loc : loc) : 'a Location.loc = { txt; loc }

(** Strips the location info from a value of type ['a loc] *)
let no_loc (a_loc : 'a Astlib.Location.loc) : 'a = a_loc.txt

(** Maps a function component-wise over a pair *)
let map2 ~f (a1, a2) = (f a1, f a2)

(** Converts a triple to a pair *)
let triple_to_pair (a, b, _) = (a, b)

(** Checks if a list is empty
    - Backwards-compatible version of [List.is_empty], 
    which is only available in OCaml 5.1 and newer *)
let list_is_empty (lst : 'a list) : bool =
  match lst with
  | [] -> true
  | _ -> false

(** Takes the disjunction of a Boolean list
    - The empty list corresponds to false
    - Reimplementation of the [or] function in 
      Haskell's [GHC.Prelude] *)
let rec list_or (xs : bool list) : bool =
  match xs with
  | [] -> false
  | x :: xs -> x || list_or xs

(** Retrieves all elements of a list except the last one *)
let rec remove_last (lst : 'a list) : 'a list =
  match lst with
  | [] | [ _ ] -> []
  | x :: xs -> x :: remove_last xs

(** Returns the final element of a list (if one exists) 
    - Raises an exception if the list is empty *)
let rec get_last (lst : 'a list) : 'a =
  match lst with
  | [] -> failwith "List is empty"
  | [ x ] -> x
  | x :: xs -> get_last xs

(** Name of the abstract type in the module signature, 
    by default ["t"] *)
let abstract_ty_name : string = "t"

(*******************************************************************************)
(** {1 Longident utility functions} *)

(** Alias for [String.uncapitalize_ascii] *)
let uncapitalize = String.uncapitalize_ascii

(** Converts a [Longident] to a regular string, *)
let string_of_lident (lident : Longident.t) : string =
  let xs = Astlib.Longident.flatten lident in
  match xs with
  | [] -> ""
  | [ x ] -> x
  | _ -> String.concat ~sep:"." xs

(** Only uncapitalizes the final [Lident] in a [Longident.t] 
    (prefixes in [Ldot]'s are left unchanged) *)
let rec uncapitalize_lident (lident : Longident.t) : Longident.t =
  match lident with
  | Lident s -> Lident (uncapitalize s)
  | Ldot (prefix, s) -> Ldot (prefix, uncapitalize s)
  | Lapply (l1, l2) -> Lapply (uncapitalize_lident l1, uncapitalize_lident l2)

(** [add_lident_prefix p l] adds the prefix [p] to the identifier [l] 
    using dot notation, returning a new identifier [p.l] *)
let add_lident_prefix (prefix : string) (lident : Longident.t) : Longident.t =
  Ldot (Lident prefix, string_of_lident (uncapitalize_lident lident))

(** [add_lident_loc_prefix] is like [add_lident_prefix], 
    but attaches location information to the resultant identifier *)
let add_lident_loc_prefix (prefix : string)
  ({ txt; loc } : Longident.t Location.loc) : Longident.t Location.loc =
  with_loc ~loc @@ add_lident_prefix prefix txt

(******************************************************************************)
(** {1 Pretty-printers} *)

(** Alias for [Format.err_formatter] *)
let err_fmt : Format.formatter = Format.err_formatter

(** Pretty-printer for [pattern]'s *)
let pp_pattern : pattern -> unit = Astlib.Pprintast.pattern err_fmt

(** Pretty-printer for [core_type]'s *)
let pp_core_type : core_type -> unit = Astlib.Pprintast.core_type err_fmt

(** Pretty-printer for [expression]'s *)
let pp_expression : expression -> unit = Astlib.Pprintast.expression err_fmt

(** Pretty-printer for [structure_item]'s *)
let pp_structure_item : structure_item -> unit =
  Astlib.Pprintast.structure_item err_fmt

(******************************************************************************)
(** {1 Utility functions for working with Ppxlib} *)

(** List of OCaml base types 
    - The named argument [loc] is necessary in order for 
    the [Ppxlib.Metaquot] quotations to expand to the appropriate 
    AST fragments representing the base types. *)
let base_types ~(loc : location) : core_type list =
  [ [%type: int];
    [%type: int32];
    [%type: int64];
    [%type: nativeint];
    [%type: char];
    [%type: bool];
    [%type: unit];
    [%type: float];
    [%type: string]
  ]

(** [pexp_ident_of_string x ~loc] creates the expression [Pexp_ident x]
    at location [loc] *)
let pexp_ident_of_string (x : string) ~(loc : location) : expression =
  pexp_ident ~loc (with_loc (Longident.parse x) ~loc)

(** [ppat_var_of_string x ~loc] creates the pattern [Ppat_var x] 
    at location [loc] *)
let ppat_var_of_string (x : string) ~(loc : location) : pattern =
  ppat_var ~loc (with_loc x ~loc)

(** [mk_cstr ~name ~loc arg_tys] creates a constructor with the [name] 
    for an algebraic data type at the location [loc] with 
    argument types [arg_tys] *)
let mk_cstr ~(name : string) ~(loc : location) ~(arg_tys : core_type list) :
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

(** Instantiates all type variables ['a] inside a type expression with [int] 
    by recursing over the structure of the type expression. 
    Base types are left unchanged. 
    Note: this function only recurses over type expressions when 
    they consist of:
    - Type constructor applications ([Ptyp_constr])
    - Tuples ([Ptyp_tuple])
    - Arrow/function types ([Ptyp_arrow]). *)
let rec monomorphize (ty : core_type) : core_type =
  let loc = ty.ptyp_loc in
  match ty.ptyp_desc with
  | ty_desc when List.mem ty ~set:(base_types ~loc) -> ty
  | Ptyp_var _ -> [%type: int]
  | Ptyp_arrow (arg_lbl, t1, t2) ->
    { ty with
      ptyp_desc = Ptyp_arrow (arg_lbl, monomorphize t1, monomorphize t2)
    }
  | Ptyp_tuple tys ->
    { ty with ptyp_desc = Ptyp_tuple (List.map ~f:monomorphize tys) }
  | Ptyp_constr (ident, ty_params) ->
    { ty with
      ptyp_desc = Ptyp_constr (ident, List.map ~f:monomorphize ty_params)
    }
  | _ -> ty

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
    corresponds to the nullary constructor [Empty]).
*)
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
    with different types when called in [get_cstr_names] *)
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
    a list consisting of (constructor name, constructor arguments, 
    typing context) constructor names (annotated with their locations) *)
let get_cstr_metadata (cstrs : constructor_declaration list) :
  (Longident.t Location.loc * pattern option * inv_ctx) list =
  List.map cstrs ~f:(fun { pcd_name = { txt; loc }; pcd_args; _ } ->
    let cstr_name = with_loc (Longident.parse txt) ~loc in
    match pcd_args with
    (* Constructors with no arguments *)
    | Pcstr_tuple [] -> (cstr_name, None, empty_ctx)
    (* N-ary constructors (where n > 0) *)
    | Pcstr_tuple arg_tys ->
      let (cstr_args, gamma) : pattern * inv_ctx =
        get_cstr_args ~loc Fun.id arg_tys in
      (cstr_name, Some cstr_args, gamma)
    | Pcstr_record arg_lbls ->
      let cstr_args, gamma =
        get_cstr_args ~loc (fun lbl_decl -> lbl_decl.pld_type) arg_lbls in
      (cstr_name, Some cstr_args, gamma))

(** Takes a [type_declaration] for an algebraic data type 
    and returns a list of (constructor name, constructor arguments) 
    - Raises an exception if the [type_declaration] doesn't correspond to an 
      algebraic data type *)
let get_cstrs_of_ty_decl (ty_decl : type_declaration) :
  (Longident.t Location.loc * pattern option) list =
  match ty_decl.ptype_kind with
  | Ptype_variant args -> List.map ~f:triple_to_pair (get_cstr_metadata args)
  | _ -> failwith "error: expected an algebraic data type definition"

(** Converts a type expression [ty] to its camel-case string representation 
    (for use as a constructor in an algebraic data type) 
    - The type expression is monomorphized prior to computing its string
    representation (i.e. ['a] is instantiated to [int]).
    - Note: polymoprhic variants, objects, extensions/attributes are 
    not supported by this function.  *)
let rec string_of_core_ty (ty : core_type) : string =
  let loc = ty.ptyp_loc in
  match ty.ptyp_desc with
  | Ptyp_var _ | Ptyp_any -> string_of_core_ty (monomorphize ty)
  | Ptyp_constr ({ txt = ident; _ }, ty_params) ->
    let ty_constr_str =
      Astlib.Longident.flatten ident
      |> String.concat ~sep:"" |> String.capitalize_ascii in
    let params_str =
      String.concat ~sep:"" (List.map ~f:string_of_core_ty ty_params) in
    params_str ^ ty_constr_str
  | Ptyp_tuple tys ->
    let ty_strs =
      List.map tys ~f:(fun ty ->
        string_of_core_ty ty |> String.capitalize_ascii) in
    String.concat ~sep:"" ty_strs ^ "Product"
  | Ptyp_arrow (_, t1, t2) -> string_of_core_ty t1 ^ string_of_core_ty t2
  | _ -> failwith "type expression not supported by string_of_core_type"

(** [mk_adt ~loc ~name constructors] creates the definition of 
    an algebraic data type called [name] at location [loc] 
    with the specified [constructors] *)
let mk_adt ~(loc : location) ~(name : string)
  ~(constructors : constructor_declaration list) : type_declaration =
  type_declaration ~loc
    ~name:{ txt = name; loc } (* Name of type *)
    ~cstrs:[] (* Type constraints, not needed here *)
    ~params:[] (* Type parameters *)
    ~kind:(Ptype_variant constructors) ~private_:Public
    ~manifest:None (* RHS of [type t =...], doesn't apply here *)

(** [mk_error ~local ~global msg] creates an error extension node, 
    associated with an element in the AST at the location [local],
    and reports the error message [msg] at the location [global] *)
let mk_error ~(local : location) ~(global : location) msg : structure_item =
  let ext = Location.error_extensionf ~loc:local msg in
  pstr_extension ~loc:global ext []

(** [attr loc name] creates an attribute called [name] at [loc] *)
let attr ~(loc : location) ~(name : string) : attribute =
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
let mk_valt_pat ?(abs_ty_parameterized = false) (x : string) ~(loc : location) :
  pattern =
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
let get_match_arm ~(loc : location) (expr_vars : string list)
  ~(abs_ty_parameterized : bool) : pattern =
  match expr_vars with
  | [] -> failwith "impossible"
  | [ x ] -> mk_valt_pat ~loc ~abs_ty_parameterized (add_prime x)
  | _ ->
    let val_exprs : pattern list =
      List.map
        ~f:(fun x -> mk_valt_pat ~loc ~abs_ty_parameterized (add_prime x))
        expr_vars in
    ppat_tuple ~loc val_exprs
