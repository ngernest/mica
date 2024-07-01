open Ppxlib
open Ast_builder.Default
open Lident
open Printers
open StdLabels
open Miscellany

(* TODO: module that contain functions for generating fresh names *)

(* TODO: replace [add_prime] with [quote]? *)

(** [pexp_ident_of_string x ~loc] creates the expression [Pexp_ident x]
    at location [loc] *)
let pexp_ident_of_string (x : string) ~(loc : Location.t) : expression =
  pexp_ident ~loc (lident_loc_of_string x ~loc)

(** [ppat_var_of_string x ~loc] creates the pattern [Ppat_var x] 
            at location [loc] *)
let ppat_var_of_string (x : string) ~(loc : Location.t) : pattern =
  ppat_var ~loc (with_loc x ~loc)

(** Produces a fresh variable at location [loc], with the type [ty]
    of the variable serialized & prefixed to the resultant variable name *)
let mk_fresh_ppat_var ~(loc : Location.t) (ty : core_type) : pattern =
  let prefix = uncapitalize (string_of_core_ty ty) in
  gen_symbol ~prefix () |> ppat_var_of_string ~loc

(** [mk_fresh_legacy ~loc i ty] generates a fresh variable at location [loc] 
    that corresponds to the type [ty], with the (integer) index [i + 1] 
    used as a varname suffix 
    - We add 1 to [i] so that variable names are 1-indexed 
    - Note: this function has now been deprecated in favor of [mk_fresh_ppat_var],
      which uses [Ppxlib]'s official [gen_symbol] function for producing
      fresh variable names
    - Note: this function is not exposed in [names.mli] to avoid 
      clients of this module from using this function *)
let rec mk_fresh_legacy ~(loc : Location.t) (i : int) (ty : core_type) : pattern
    =
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
      | Ptyp_constr ({ txt; _ }, _) ->
        let tyconstr = string_of_lident txt in
        if String.equal tyconstr "list" then "lst"
          (* For unrecognized type constructors, just extract the first char of
             the type constructor's name *)
        else String.sub tyconstr ~pos:0 ~len:1
      | _ ->
        pp_core_type ty;
        failwith "TODO: [mk_fresh] not supported for types of this shape") in
  ppat_var ~loc (with_loc ~loc (varname ^ Int.to_string (i + 1)))

(** Produces fresh variable names corresponding to a value [arg] of type 
    [constructor_arguments] at location [loc] *)
let varnames_of_cstr_args ~(loc : Location.t) (arg : constructor_arguments) :
  pattern list =
  match arg with
  | Pcstr_tuple tys -> List.map ~f:(mk_fresh_ppat_var ~loc) tys
  | Pcstr_record lbl_decls ->
    List.map lbl_decls ~f:(fun { pld_name; pld_type; pld_loc; _ } ->
        gen_symbol ~prefix:pld_name.txt () |> ppat_var_of_string ~loc:pld_loc)

let ppat_construct_of_cstr_decl ~(loc : Location.t)
  (cstr_decl : constructor_declaration) =
  let cstr_name : Longident.t loc =
    map_with_loc ~f:Longident.parse cstr_decl.pcd_name in
  let arg_names = varnames_of_cstr_args cstr_decl.pcd_args in
  (* TODO: figure out what we want to do here *)
  ppat_construct ~loc cstr_name
