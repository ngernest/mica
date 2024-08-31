open Alcotest
open Ppxlib
open Ppx_mica__Utils
open StdLabels

(******************************************************************************)
(* Alcotest helpers *)

(** Creates an Alcotest test case that compares the [expected] and [actual] 
    values, using the provided [testable] & [name]
    - This helper function removes the need to separately define assertions & 
      test cases using [check] & [test_case] respectively *)
let mk_test (testable : 'a testable) (name : string) (expected : 'a)
  (actual : 'a) : return test_case =
  let test_to_run () = check testable name expected actual in
  test_case name `Quick test_to_run

(******************************************************************************)
(* Boilerplate for making [core_type] a [testable] type in the Alcotest
   harness *)

let pp_core_type = Ppxlib.Pprintast.core_type

let core_type_eq (t1 : core_type) (t2 : core_type) : bool =
  String.equal (Ppxlib.string_of_core_type t1) (Ppxlib.string_of_core_type t2)

let core_ty_list_eq (tys : core_type list) (tys' : core_type list) : bool =
  List.equal ~eq:core_type_eq tys tys'

let core_ty_testable : core_type testable = testable pp_core_type core_type_eq
let core_ty_list_testable : core_type list testable = list core_ty_testable

(******************************************************************************)
(* Boilerplate for [constructor_declaration testable] *)

(** Pretty printer for the [constructor_declaration] type *)
let pp_constr_decl (ppf : Stdlib.Format.formatter)
  (cd : constructor_declaration) : unit =
  match cd.pcd_args with
  | Pcstr_tuple tys ->
    let args =
      String.concat ~sep:" * " (List.map ~f:string_of_monomorphized_ty tys)
    in
    Fmt.pf ppf "%s of %s\n" (no_loc cd.pcd_name) args
  | Pcstr_record _ -> failwith "Pcstr_record not supported"

(** Equality function for [constructor_declaration]'s *)
let constr_decl_eq (cd1 : constructor_declaration)
  (cd2 : constructor_declaration) : bool =
  let name1, name2 = map2 ~f:no_loc (cd1.pcd_name, cd2.pcd_name) in
  String.equal name1 name2
  &&
  match (cd1.pcd_args, cd2.pcd_args) with
  | Pcstr_tuple args1, Pcstr_tuple args2 -> core_ty_list_eq args1 args2
  | Pcstr_tuple _, Pcstr_record _ | Pcstr_record _, Pcstr_tuple _ -> false
  | _, _ -> failwith "Not equal, Pcstr_record not supported"

let constr_decl_testable : constructor_declaration testable =
  testable pp_constr_decl constr_decl_eq

let constr_decl_list_testable : constructor_declaration list testable =
  list constr_decl_testable

(******************************************************************************)
(** Boilerplate for [Longident.t testable] *)

(** Pretty-printer for [Longident.t] *)
let pp_lident (ppf : Stdlib.Format.formatter) (lident : Longident.t) : unit =
  Fmt.pf ppf "%s" (string_of_lident lident)

(** Equality for [Longident.t]'s is based on induction over the structure 
        of the [Longident] *)
let rec lident_eq (l1 : Longident.t) (l2 : Longident.t) : bool =
  match (l1, l2) with
  | Lident s1, Lident s2 -> String.equal s1 s2
  | Ldot (p1, s1), Ldot (p2, s2) -> lident_eq p1 p2 && String.equal s1 s2
  | Lapply (l11, l12), Lapply (l21, l22) ->
    lident_eq l11 l21 && lident_eq l12 l22
  | _, _ -> false

let lident_testable : Longident.t testable = testable pp_lident lident_eq
