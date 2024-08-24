open Ppx_mica__Utils
open Ppx_mica__Type_deriver
open Boilerplate
open StdLabels
open Alcotest

let loc = Location.none

(******************************************************************************)
(* Tests for [mk_ty_cstrs] *)

let mk_ty_cstrs_single_base_ty () =
  let sig_items = [%sig: val x : int] in
  let expected = mk_cstr ~name:"Int" ~loc ~arg_tys:[] in
  mk_test constr_decl_list_testable "1 base type (int)" (mk_ty_cstrs sig_items)
    [ expected ]

let mk_ty_cstrs_single_mono_abs_ty () =
  let sig_items = [%sig: val x : t] in
  let expected = mk_cstr ~name:"T" ~loc ~arg_tys:[] in
  mk_test constr_decl_list_testable "1 mono abs type" (mk_ty_cstrs sig_items)
    [ expected ]

let mk_ty_cstrs_single_poly_abs_ty () =
  let sig_items = [%sig: val x : 'a t] in
  let expected = mk_cstr ~name:"IntT" ~loc ~arg_tys:[] in
  mk_test constr_decl_list_testable "1 poly abs type" (mk_ty_cstrs sig_items)
    [ expected ]

let mk_ty_cstrs_two_base () =
  let sig_items =
    [%sig:
      val x : int
      val y : string] in
  let expected =
    List.map ~f:(fun name -> mk_cstr ~name ~loc ~arg_tys:[]) [ "Int"; "String" ]
  in
  mk_test constr_decl_list_testable "2 constructors" (mk_ty_cstrs sig_items)
    expected

let mk_ty_cstrs_no_dupes () =
  let sig_items =
    [%sig:
      val x : int
      val y : string
      val z : int] in
  let expected =
    List.map ~f:(fun name -> mk_cstr ~name ~loc ~arg_tys:[]) [ "Int"; "String" ]
  in
  mk_test constr_decl_list_testable "no duplicates" (mk_ty_cstrs sig_items)
    expected

(******************************************************************************)
(* Tests for [get_cstr_arity] *)

let get_cstr_arity_nullary () =
  let cstr = mk_cstr ~name:"C" ~loc ~arg_tys:[] in
  mk_test int "nullary" (get_cstr_arity cstr) 0

let get_cstr_arity_unary () =
  let cstr = mk_cstr ~name:"C1" ~loc ~arg_tys:[ [%type: int] ] in
  mk_test int "unary" (get_cstr_arity cstr) 1

let get_cstr_arity_binary () =
  let cstr = mk_cstr ~name:"C2" ~loc ~arg_tys:[ [%type: int]; [%type: bool] ] in
  mk_test int "binary" (get_cstr_arity cstr) 2

let get_cstr_arity_ternary () =
  let cstr =
    mk_cstr ~name:"C3" ~loc
      ~arg_tys:[ [%type: int]; [%type: bool]; [%type: char] ] in
  mk_test int "ternary" (get_cstr_arity cstr) 3

(******************************************************************************)
(* Tests for [is_base_case] *)

let is_base_case_nullary_cstr () =
  (* We create a constructor [Empty] *)
  let cstr = mk_cstr ~name:"Empty" ~loc ~arg_tys:[] in
  mk_test bool "nullary cstr" (is_base_case cstr) true

let is_base_case_unary_cstr_ok () =
  (* We create a constructor [Create of unit] *)
  let cstr = mk_cstr ~name:"Create" ~loc ~arg_tys:[ [%type: unit] ] in
  mk_test bool "unary cstr w/ unit arg" (is_base_case cstr) true

let is_base_case_unary_cstr_bad () =
  (* We create a constructor [Duplicate of expr] *)
  let cstr = mk_cstr ~name:"Duplicate" ~loc ~arg_tys:[ [%type: expr] ] in
  mk_test bool "unary cstr w/ expr arg" (is_base_case cstr) false

let is_base_case_binary_cstr_ok () =
  (* We create a constructor [Power of int * int] *)
  let cstr =
    mk_cstr ~name:"Power" ~loc ~arg_tys:[ [%type: int]; [%type: int] ] in
  mk_test bool "binary cstr w/ int * int args" (is_base_case cstr) true
