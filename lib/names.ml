open Ppxlib
open Ast_builder.Default
open Lident
open Printers
open StdLabels
open Miscellany

(******************************************************************************)
(** {1 Quoting names} *)

(** Turns the variable [x] into [x'] *)
let add_prime : string -> string = fun x -> x ^ "\'"

(** [update_expr_arg_names expr_args args] replaces each variable [x] in 
    [expr_args] if [x'] (the variable with a prime added) is in [expr_args] *)
let update_expr_arg_names (expr_args : string list) (args : string list) :
  string list =
  List.map args ~f:(fun x ->
      if List.mem (add_prime x) ~set:expr_args then add_prime x else x)

(******************************************************************************)
(** {1 Producing fresh identifiers} *)

(** Produces a fresh identifier of type at location [loc], with the type [ty]
    of the variable serialized & prefixed to the resultant variable name 
    - Note that the type of the resultant identifier is polymorphic:
      The function argument [f] specifies how to take a [Location.t]
       and a [string] to form the desired identifier type. *)
let mk_fresh ~(loc : Location.t) ~(f : loc:Location.t -> string -> 'a)
  (ty : core_type) : 'a =
  let prefix = uncapitalize (string_of_monomorphized_ty ty) in
  f ~loc (gen_symbol ~prefix ())

(** Makes a fresh identifier of type [pattern] (a [Ppat_var]) for type [ty]
    - [mk_fresh_pvar] is [mk_fresh], specialized to [pattern]s *)
let mk_fresh_pvar ~(loc : Location.t) (ty : core_type) : pattern =
  mk_fresh ~loc ty ~f:pvar

(** Makes a fresh identifier of type [expression] (a [Pexp_ident]) for type [ty]
    - [mk_fresh_pvar] is [mk_fresh], specialized to [expression]s *)
let mk_fresh_evar ~(loc : Location.t) (ty : core_type) : expression =
  mk_fresh ~loc ty ~f:evar

(** Produces fresh identifiers for [args] at [loc] 
    - Like [mk_fresh], the resultant identifier type is polymorphic: 
      The function argument [f] specifies how to take a [Location.t]
      and a [string] to form the desired identifier type. *)
let varnames_of_cstr_args ~(loc : Location.t) (args : constructor_arguments)
  ~(f : loc:Location.t -> string -> 'a) : 'a list =
  match args with
  | Pcstr_tuple tys -> List.map ~f:(fun ty -> mk_fresh ~loc ~f ty) tys
  | Pcstr_record lbl_decls ->
    List.map lbl_decls ~f:(fun { pld_name; pld_loc; _ } ->
        gen_symbol ~prefix:pld_name.txt () |> f ~loc:pld_loc)

(** Takes [constructor_arguments] and produces a list of fresh identifiers 
    which each have type [pattern] 
    - This function is [varnames_of_cstr_args], specialized to [pattern]s *)
let pvars_of_cstr_args ~(loc : Location.t) (args : constructor_arguments) :
  pattern list =
  varnames_of_cstr_args ~loc args ~f:pvar

(** Takes [constructor_arguments] and produces a list of fresh identifiers 
    which each have type [expression] 
    - This function is [varnames_of_cstr_args], specialized to [expression]s *)
let evars_of_cstr_args ~(loc : Location.t) (args : constructor_arguments) :
  expression list =
  varnames_of_cstr_args ~loc args ~f:evar

(******************************************************************************)
(** {1 Convering between different identifier types} *)

(** Takes a [constructor_declaration] and produces the pattern 
    [Ppat_construct] *)
let ppat_construct_of_cstr_decl ~(loc : Location.t)
  (cstr_decl : constructor_declaration) : pattern =
  let cstr_name : Longident.t loc =
    map_with_loc ~f:Longident.parse cstr_decl.pcd_name in
  (* Generate fresh names for the construct arguments, then convert them to the
     [Ppat_tuple] pattern *)
  let arg_names : pattern list = pvars_of_cstr_args ~loc cstr_decl.pcd_args in
  let cstr_args : pattern option = ppat_tuple_opt ~loc arg_names in
  ppat_construct ~loc cstr_name cstr_args
