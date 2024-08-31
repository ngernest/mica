open Ppx_mica__Utils
open Boilerplate
open Ppxlib
open Alcotest

let loc = Location.none

(******************************************************************************)
(* Tests for [equal_core_type_ty_cstr] *)

(** Helper function: constructs an Alcotest test case which checks
    whether [core_ty] & [cstr_name] are equal *)
let equal_core_type_cstr_name (core_ty : core_type) (cstr_name : string) =
  let cstr = mk_cstr ~name:cstr_name ~loc ~arg_tys:[] in
  let core_ty_name = Ppxlib.string_of_core_type core_ty in
  let test_case_name = Format.sprintf "%s = %s" core_ty_name cstr_name in
  mk_test bool test_case_name (equal_ty_cstr_core_type cstr core_ty) true

let equal_core_ty_ty_cstr_bool_Bool () =
  equal_core_type_cstr_name [%type: bool] "Bool"

let equal_core_ty_ty_cstr_int_Int () =
  equal_core_type_cstr_name [%type: int] "Int"

let equal_core_ty_ty_cstr_t_T () = equal_core_type_cstr_name [%type: t] "T"

let equal_core_ty_ty_cstr_alpha_Int () =
  equal_core_type_cstr_name [%type: 'a] "Int"

let equal_core_ty_ty_cstr_alpha_t_IntT () =
  equal_core_type_cstr_name [%type: 'a t] "IntT"

let equal_core_ty_ty_cstr_alpha_int_list_IntList () =
  equal_core_type_cstr_name [%type: int list] "IntList"

let equal_core_ty_ty_cstr_alpha_string_option_StringOption () =
  equal_core_type_cstr_name [%type: string option] "StringOption"

let equal_core_ty_ty_cstr_product_type () =
  equal_core_type_cstr_name [%type: int * bool] "IntBoolProduct"

let equal_core_type_ty_cstr_function_type () =
  equal_core_type_cstr_name [%type: bool -> int] "BoolInt"

(******************************************************************************)
(* Tests for [equal_core_type] *)

let equal_core_type_any_refl () =
  mk_test bool "_ = _" (equal_core_type [%type: _] [%type: _]) true

let equal_core_type_int_refl () =
  mk_test bool "int = int" (equal_core_type [%type: int] [%type: int]) true

let equal_core_type_int_bool_neq () =
  mk_test bool "int != bool" (equal_core_type [%type: int] [%type: bool]) false

let equal_core_type_int_string_product_refl () =
  mk_test bool "int * string = int * string"
    (equal_core_type [%type: int * string] [%type: int * string])
    true

let equal_core_type_int_string_product_permute () =
  mk_test bool "int * string != string * int"
    (equal_core_type [%type: int * string] [%type: string * int])
    false

let equal_core_type_string_list_refl () =
  mk_test bool "string list = string list"
    (equal_core_type [%type: string list] [%type: string list])
    true

let equal_core_type_different_list_types () =
  mk_test bool "string list != bool list"
    (equal_core_type [%type: string list] [%type: bool list])
    false

let equal_core_type_nested_option_list_refl () =
  mk_test bool "int option list = int option list"
    (equal_core_type [%type: int option list] [%type: int option list])
    true

let equal_core_type_function_types_refl () =
  mk_test bool "char -> bool = char -> bool"
    (equal_core_type [%type: char -> bool] [%type: char -> bool])
    true

let equal_core_type_alpha_t_refl () =
  mk_test bool "'a t = 'a t" (equal_core_type [%type: 'a t] [%type: 'a t]) true

let equal_core_type_alpha_beta_t_neq () =
  mk_test bool "'a t != 'b t"
    (equal_core_type [%type: 'a t] [%type: 'b t])
    false

(******************************************************************************)
(* Tests for [equal_constructor_declaration] *)

let equal_constructor_declaration_enum_refl () =
  let c1 = mk_cstr ~name:"C" ~loc ~arg_tys:[] in
  let c2 = mk_cstr ~name:"C" ~loc ~arg_tys:[] in
  mk_test bool "C = C" (equal_constructor_declaration c1 c2) true

let equal_constructor_declaration_unary_refl () =
  let c1 = mk_cstr ~name:"C" ~loc ~arg_tys:[ [%type: int] ] in
  let c2 = mk_cstr ~name:"C" ~loc ~arg_tys:[ [%type: int] ] in
  mk_test bool "C of int = C of int" (equal_constructor_declaration c1 c2) true

let equal_constructor_declaration_unary_diff_names () =
  let c1 = mk_cstr ~name:"C1" ~loc ~arg_tys:[ [%type: int] ] in
  let c2 = mk_cstr ~name:"C2" ~loc ~arg_tys:[ [%type: int] ] in
  mk_test bool "C1 of int != C2 of int"
    (equal_constructor_declaration c1 c2)
    false

let equal_constructor_declaration_unary_same_name_diff_arg_types () =
  let c1 = mk_cstr ~name:"C" ~loc ~arg_tys:[ [%type: int] ] in
  let c2 = mk_cstr ~name:"C" ~loc ~arg_tys:[ [%type: string] ] in
  mk_test bool "C of int != C of string"
    (equal_constructor_declaration c1 c2)
    false

let equal_constructor_declaration_binary_refl () =
  let name = "C" in
  let arg_tys = [ [%type: int]; [%type: string] ] in
  let c1 = mk_cstr ~name ~loc ~arg_tys in
  let c2 = mk_cstr ~name ~loc ~arg_tys in
  mk_test bool "C of int * string = C of int * string"
    (equal_constructor_declaration c1 c2)
    true

let equal_constructor_declaration_binary_diff_names () =
  let arg_tys = [ [%type: int]; [%type: string] ] in
  let c1 = mk_cstr ~name:"C1" ~loc ~arg_tys in
  let c2 = mk_cstr ~name:"C2" ~loc ~arg_tys in
  mk_test bool "C1 of int * string != C2 of int * string"
    (equal_constructor_declaration c1 c2)
    false

let equal_constructor_declaration_binary_permute_args () =
  let name = "C" in
  let c1 = mk_cstr ~name ~loc ~arg_tys:[ [%type: int]; [%type: string] ] in
  let c2 = mk_cstr ~name ~loc ~arg_tys:[ [%type: string]; [%type: int] ] in
  mk_test bool "C of int * string != C of string * int"
    (equal_constructor_declaration c1 c2)
    false
