open Ppxlib
open Ast_builder.Default
open Lident
open Printers
open StdLabels
open Miscellany

(** Turns the variable [x] into [x'] *)
let add_prime : string -> string = fun x -> x ^ "\'"

(** A more elaborate version of [add_prime] which does the same thing,
    but uses [Ppxlib]'s in-built [quoter]
    - Note: this function is currently unused
    - TODO: the quoter ought to be created by the caller, methinks *)
let quote_name (name : string) : string =
  let open Expansion_helpers.Quoter in
  let quoter = create () in
  let new_name = quote quoter (evar ~loc:Location.none name) in
  match new_name.pexp_desc with
  | Pexp_ident { txt = quoted_name; _ } -> string_of_lident quoted_name
  | _ -> failwith "impossible"

(** Produces a fresh variable at location [loc], with the type [ty]
    of the variable serialized & prefixed to the resultant variable name *)
(* let mk_fresh_ppat_var ~(loc : Location.t) (ty : core_type) : pattern =
  let prefix = uncapitalize (string_of_core_ty ty) in
  gen_symbol ~prefix () |> pvar ~loc *)

let mk_fresh ~(loc : Location.t) ~(f : loc:Location.t -> string -> 'a) (ty : core_type) : 'a = 
  let prefix = uncapitalize (string_of_core_ty ty) in
  f ~loc (gen_symbol ~prefix ())

let mk_fresh_pvar ~(loc : Location.t) (ty : core_type) : pattern = 
  mk_fresh ~loc ty ~f:pvar
  
let mk_fresh_evar ~(loc : Location.t) (ty : core_type) : expression = 
  mk_fresh ~loc ty ~f:evar 

(** Produces fresh variable names corresponding to a value [arg] of type 
    [constructor_arguments] at location [loc] *)
let varnames_of_cstr_args ~(loc : Location.t) (args : constructor_arguments) 
  ~(f : loc:Location.t -> string -> 'a) : 'a list =
  match args with
  | Pcstr_tuple tys -> List.map ~f:(fun ty -> mk_fresh ~loc ~f ty) tys
  | Pcstr_record lbl_decls ->
    List.map lbl_decls ~f:(fun { pld_name; pld_type; pld_loc; _ } ->
        gen_symbol ~prefix:pld_name.txt () |> f ~loc:pld_loc)

let pvars_of_cstr_args ~(loc : Location.t) (args : constructor_arguments) : pattern list = 
  varnames_of_cstr_args ~loc args ~f:pvar 

let evars_of_cstr_args ~(loc : Location.t) (args : constructor_arguments) : expression list = 
  varnames_of_cstr_args ~loc args ~f:evar


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

(** [update_expr_arg_names expr_args args] replaces each variable [x] in 
    [expr_args] if [x'] (the variable with a prime added) is in [expr_args] *)
let update_expr_arg_names (expr_args : string list) (args : string list) :
  string list =
  List.map args ~f:(fun x ->
      if List.mem (add_prime x) ~set:expr_args then add_prime x else x)
