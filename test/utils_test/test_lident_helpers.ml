open Ppx_mica__Utils
open Boilerplate
open Alcotest
open Ppxlib

(******************************************************************************)

(** Testing [string_of_lident] *)
let string_of_lident_trivial () =
  mk_test string "lident" (string_of_lident (Lident "M")) "M"

let string_of_lident_ldot () =
  mk_test string "ldot"
    (string_of_lident (Ldot (Lident "M", "empty")))
    "M.empty"

let string_of_lident_nested_lot () =
  mk_test string "nested ldots"
    (string_of_lident (Longident.parse "M1.M2.empty"))
    "M1.M2.empty"

(******************************************************************************)
(** Testing [uncapitalize_lident] *)

let uncapitalize_lident_trivial () =
  mk_test lident_testable "lident"
    (uncapitalize_lident (Lident "Module"))
    (Lident "module")

let uncapitalize_lident_ldot () =
  let actual = uncapitalize_lident (Longident.parse "M.Empty") in
  let expected = Longident.parse "M.empty" in
  mk_test lident_testable "ldot" expected actual

let uncapitalize_lident_ldot_nested () =
  let actual = uncapitalize_lident (Longident.parse "M1.M2.Empty") in
  let expected = Longident.parse "M1.M2.empty" in
  mk_test lident_testable "nested ldots" expected actual

let uncapitalize_lident_ldot_doubly_nested () =
  let actual = uncapitalize_lident (Longident.parse "M1.M2.M3.Empty") in
  let expected = Longident.parse "M1.M2.M3.empty" in
  mk_test lident_testable "doubly-nested ldots" expected actual

(******************************************************************************)

(** Testing [add_lident_prefix] *)
let add_lident_prefix_mod_path () =
  mk_test lident_testable "lident"
    (add_lident_prefix "M" (Lident "empty"))
    (Longident.parse "M.empty")

let add_lident_prefix_ldot () =
  mk_test lident_testable "ldot"
    (add_lident_prefix "M1" @@ Ldot (Lident "M2", "empty"))
    (Ldot (Lident "M1", "M2.empty"))
