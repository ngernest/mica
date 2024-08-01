open Ppxlib
open Ast_builder.Default
open Utils
open Type_deriver
open StdLabels

(** Produces a single pattern of the form [ValInt x1], 
    where [val_cstr_pat] is the name of the [value] constructor 
    and [arg] is the name of the constructor argument *)
let mk_val_cstr_app ~loc (val_cstr_pat : Longident.t Location.loc)
  (arg : string) : pattern =
  ppat_construct ~loc val_cstr_pat (Some (ppat_tuple ~loc [ pvar ~loc arg ]))

(*** Produces a pattern of the form [(ValInt x1, ValInt x2)] - [x1, x2] are the
  names for the two vairables - [value_cstr] is the name of the constructor for
  the [value] type *)
let mk_val_cstr_app2 ~loc (value_cstr : string) ((x1, x2) : string * string) :
  pattern =
  let val_cstr_pat = with_loc ~loc (Longident.parse value_cstr) in
  ppat_tuple ~loc
    [ mk_val_cstr_app ~loc val_cstr_pat x1;
      mk_val_cstr_app ~loc val_cstr_pat x2
    ]

(** Produces a fresh constructor argument with the name of the type 
    as a prefix *)
let mk_fresh_cstr_arg (ty : core_type) : string =
  let prefix = uncapitalize (string_of_monomorphized_ty ty) in
  gen_symbol ~prefix ()

(** Produces a test function (eg [test_int]), where:
   - [ty] is the concrete type at which observational equivalence is being tested
   - [ty_cstr] is the corresponding constructor in the [ty] datatype
   - [value_cstr] is the corresponding constructor of the [value] datatype 
   - [test_name] is the name of the test (eg [test_int] )*)
let produce_test ~loc (ty : core_type) (ty_cstr : string) (value_cstr : string)
  (test_name : pattern) : structure_item =
  let x1, x2 = (mk_fresh_cstr_arg ty, mk_fresh_cstr_arg ty) in
  let val_cstr = mk_val_cstr_app2 ~loc value_cstr (x1, x2) in

  [%stri
    let [%p test_name] =
     fun [%p punit ~loc] ->
      Quickcheck.test
        (gen_expr [%e evar ~loc ty_cstr])
        ~f:(fun e ->
          match (I1.interp e, I2.interp e) with
          | [%p val_cstr] -> ())]

let derive_test_functions ~loc sig_items : structure_item list =
  let concrete_tys = uniq_ret_tys sig_items in
  let ty_cstr_names = List.map ~f:string_of_monomorphized_ty concrete_tys in
  let _value_cstr_names =
    List.map
      ~f:(Expansion_helpers.mangle ~fixpoint:"" (Prefix "Val"))
      ty_cstr_names in
  let _test_names =
    List.map
      ~f:(fun ty -> pvar ~loc (Ppxlib.string_of_core_type ty))
      concrete_tys in
  failwith
    "TODO: figure out how to do a [map3] over [ty_cstr_names, \
     value_cstr_names, test_names]"

(** Derives the [TestHarness] functor *)
let generate_functor ~(ctxt : Expansion_context.Deriver.t)
  (mt_decl : module_type_declaration) : structure =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  match mt_decl with
  | { pmtd_type = Some mod_type; pmtd_name; _ } -> (
    let sig_name = pmty_ident ~loc (map_with_loc ~f:Longident.parse pmtd_name) in
    match mod_type.pmty_desc with
    | Pmty_signature _sig_items ->
      (* let _ = derive_test_functions ~loc sig_items in *)
      [%str
        module TestHarness (M1 : [%m sig_name]) (M2 : [%m sig_name]) = struct
          module I1 = Interpret (M1)
          module I2 = Interpret (M2)
          open Core
        end]
    | _ ->
      Location.raise_errorf ~loc
        "Expected a module signature of the form [sig ... end]")
  | { pmtd_type = None; _ } ->
    Location.raise_errorf ~loc
      "Can't derive for expressions that aren't module type declarations"
