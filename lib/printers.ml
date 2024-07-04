open Ppxlib
open StdLabels
open Astlib.Pprintast
open Miscellany

(** {1 Pretty-printers} *)

(** List of OCaml base types 
    - The named argument [loc] is necessary in order for 
    the [Ppxlib.Metaquot] quotations to expand to the appropriate 
    AST fragments representing the base types. *)
let base_types ~(loc : Location.t) : core_type list =
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

(** Alias for [Format.err_formatter] *)
let err_fmt : Format.formatter = Format.err_formatter

(** Pretty-printer for [pattern]s *)
let pp_pattern : pattern -> unit = pattern err_fmt

(** Pretty-printer for [core_type]s *)
let pp_core_type : core_type -> unit = core_type err_fmt

(** Pretty-printer for [expression]s *)
let pp_expression : expression -> unit = expression err_fmt

(** Pretty-printer for [structure_item]s *)
let pp_structure_item : structure_item -> unit = structure_item err_fmt

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

(** Converts a type expression [ty] to its capitalized, camel-case 
    string representation (for use as a constructor in an algebraic data type) 
    - The type expression is monomorphized prior to computing its string
    representation (i.e. ['a] is instantiated to [int]).
    - Note: polymoprhic variants, objects, extensions/attributes are 
    not supported by this function.  
    - Note: this function is slightly different from [Ppxlib.string_of_core_type]
    due to its capitalization, camel-case & monomorphization functionalities. *)
let rec string_of_core_ty (ty : core_type) : string =
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
