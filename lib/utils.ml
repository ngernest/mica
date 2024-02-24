open Ppxlib
open Ast_helper
open Ast_builder.Default
open StdLabels

(* Suppress "unused value" compiler warnings *)
[@@@ocaml.warning "-26-27-32-33-34"]

(*******************************************************************************)
(** {1 Miscellany} *)

(** Constructs a [loc] given some payload [txt] and a location [loc] *)
let with_loc (txt : 'a) ~(loc : loc) : 'a Location.loc = { txt; loc }

(** Strips the location info from a value of type ['a loc] *)
let no_loc (a_loc : 'a Astlib.Location.loc) : 'a = a_loc.txt

(** Maps a function component-wise over a pair *)
let map2 ~f (a1, a2) = (f a1, f a2)

(** Retrieves all elements of a list except the last one *)
let rec remove_last (lst : 'a list) : 'a list =
  match lst with [] | [ _ ] -> [] | x :: xs -> x :: remove_last xs

(** Returns the final element of a list (if one exists) 
    - Raises an exception if the list is empty *)
let rec get_last (lst : 'a list) : 'a =
  match lst with
  | [] ->
      failwith "List is empty"
  | [ x ] ->
      x
  | x :: xs ->
      get_last xs

(******************************************************************************)
(** {1 Pretty-printers } *)

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
  [
    [%type: int];
    [%type: int32];
    [%type: int64];
    [%type: nativeint];
    [%type: char];
    [%type: bool];
    [%type: unit];
    [%type: float];
    [%type: string];
  ]

(** [mk_constructor ~name ~loc arg_tys] creates a constructor with the [name] 
    for an algebraic data type at the location [loc] with 
    argument types [arg_tys] *)
let mk_constructor ~(name : string) ~(loc : location)
    ~(arg_tys : core_type list) : constructor_declaration =
  {
    (* Constructor name *)
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
    pcd_attributes = [];
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
  | ty_desc when List.mem ty ~set:(base_types ~loc) ->
      ty
  | Ptyp_var _ ->
      [%type: int]
  | Ptyp_arrow (arg_lbl, t1, t2) ->
      {
        ty with
        ptyp_desc = Ptyp_arrow (arg_lbl, monomorphize t1, monomorphize t2);
      }
  | Ptyp_tuple tys ->
      { ty with ptyp_desc = Ptyp_tuple (List.map ~f:monomorphize tys) }
  | Ptyp_constr (ident, ty_params) ->
      {
        ty with
        ptyp_desc = Ptyp_constr (ident, List.map ~f:monomorphize ty_params);
      }
  | _ ->
      ty

(** [get_type_varams td] extracts the type parameters 
    from the type declaration [td]
    - Type variables (e.g. ['a]) are instantiated with [int] *)
let get_type_params (td : type_declaration) : core_type list =
  List.map td.ptype_params ~f:(fun (core_ty, _) -> monomorphize core_ty)

(** [mk_fresh ~loc i ty] generates a fresh variable at location [loc] 
    that corresponds to the type [ty], with the (integer) index [i] 
    as a varname suffix *)
let rec mk_fresh ~(loc : Location.t) (i : int) (ty : core_type) : pattern =
  let varname =
    begin
      match ty with
      | [%type: int] ->
          "n"
      | [%type: bool] ->
          "b"
      | [%type: char] ->
          "c"
      | [%type: string] ->
          "s"
      | [%type: unit] ->
          "u"
      | [%type: 'a] ->
          "x"
      | [%type: t] | [%type: 'a t] ->
          "e"
      | _ ->
          failwith "TODO: handle lists + tuples etc"
    end
  in
  ppat_var ~loc (with_loc ~loc (varname ^ Int.to_string i))

(** Helper function: [get_constructor_args loc get_ty args] takes [args], a list containing
    the {i representation} of constructor arguments, applies the function 
    [get_ty] to each element of [args] and produces a formatted tuple of 
    constructor arguments (using the [ppat_tuple] smart constructor for the 
    [pattern] type).  
    - Note that [args] has type ['a list], i.e. the representation of 
    constructor arguments is polymorphic -- this function is instantiated 
    with different types when called in [get_constructor_names] *)
let get_constructor_args ~(loc : Location.t) (get_ty : 'a -> core_type)
    (args : 'a list) : pattern =
  let arg_tys = List.map ~f:get_ty args in
  let arg_names = List.mapi ~f:(mk_fresh ~loc) arg_tys in
  ppat_tuple ~loc arg_names

(** Takes a list of [constructor_declaration]'s and returns 
    a list of the constructor names (annotated with their locations) *)
let get_constructor_names (cstrs : constructor_declaration list) :
    (Longident.t Location.loc * pattern) list =
  List.map cstrs ~f:(fun { pcd_name = { txt; loc }; pcd_args; _ } ->
      let cstr_name = with_loc (Longident.parse txt) ~loc in
      begin
        match pcd_args with
        | Pcstr_tuple arg_tys ->
            let cstr_args = get_constructor_args ~loc Fun.id arg_tys in
            (cstr_name, cstr_args)
        | Pcstr_record arg_lbls ->
            let cstr_args =
              get_constructor_args ~loc
                (fun lbl_decl -> lbl_decl.pld_type)
                arg_lbls
            in
            (cstr_name, cstr_args)
      end)

(** TODO: DEPRECATED, remove *)
let get_constructor_names_old (cstrs : constructor_declaration list) :
    Longident.t Location.loc list =
  List.map cstrs ~f:(fun { pcd_name = { txt; loc }; _ } ->
      with_loc (Longident.parse txt) ~loc)

(** Converts a type expression [ty] to its camel-case string representation 
    (for use as a constructor in an algebraic data type) 
    - The type expression is monomorphized prior to computing its string
    representation (i.e. ['a] is instantiated to [int]).
    - Note: polymoprhic variants, objects, extensions/attributes are 
    not supported by this function.  *)
let rec string_of_core_ty (ty : core_type) : string =
  let loc = ty.ptyp_loc in
  begin
    match ty.ptyp_desc with
    | Ptyp_var _ | Ptyp_any ->
        string_of_core_ty (monomorphize ty)
    | Ptyp_constr ({ txt = ident; _ }, ty_params) ->
        let ty_constr_str =
          Astlib.Longident.flatten ident
          |> String.concat ~sep:"" |> String.capitalize_ascii
        in
        let params_str =
          String.concat ~sep:"" (List.map ~f:string_of_core_ty ty_params)
        in
        params_str ^ ty_constr_str
    | Ptyp_tuple tys ->
        let ty_strs =
          List.map tys ~f:(fun ty ->
              string_of_core_ty ty |> String.capitalize_ascii)
        in
        String.concat ~sep:"" ty_strs ^ "Product"
    | Ptyp_arrow (_, t1, t2) ->
        string_of_core_ty t1 ^ string_of_core_ty t2
    | _ ->
        failwith "type expression not supported by string_of_core_type"
  end

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

(** Name of the abstract type in the module signature, 
    by default ["t"] *)
let abstract_ty_name : string ref = ref "t"

(** [attr loc name] creates an attribute called [name] at [loc] *)
let attr ~(loc : location) ~(name : string) =
  attribute ~loc ~name:{ txt = "deriving"; loc }
    ~payload:
      (PStr
         [
           {
             pstr_desc =
               Pstr_eval (pexp_ident ~loc { txt = Lident name; loc }, []);
             pstr_loc = loc;
           };
         ])
