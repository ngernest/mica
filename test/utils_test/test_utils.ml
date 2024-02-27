open Lib__Utils
open Lib
open Ppxlib
open StdLabels
open Alcotest

(*******************************************************************************)
(* Boilerplate for making [core_type] a [testable] type in the Alcotest
   harness *)

let pp_core_type = Ppxlib.Pprintast.core_type

let core_type_eq (t1 : core_type) (t2 : core_type) : bool =
  String.equal (Ppxlib.string_of_core_type t1) (Ppxlib.string_of_core_type t2)

let core_ty_list_eq (tys : core_type list) (tys' : core_type list) : bool =
  List.equal ~eq:core_type_eq tys tys'

let core_ty_testable : core_type testable = testable pp_core_type core_type_eq
let core_ty_list_testable : core_type list testable = list core_ty_testable

(*******************************************************************************)
(* Boilerplate for [type_declaration testable] (currently unused) *)

let pp_ty_decl = Ppxlib.Pprintast.type_declaration

(** Equality of [type_declaration]'s is based on their string representations *)
let ty_decl_eq
  ({ ptype_name = ty1; ptype_params = ty_params1; _ } : type_declaration)
  ({ ptype_name = ty2; ptype_params = ty_params2; _ } : type_declaration) : bool
    =
  let ty_args1, ty_args2 = map2 ~f:(List.map ~f:fst) (ty_params1, ty_params2) in
  String.equal (no_loc ty1) (no_loc ty2)
  && List.equal ~eq:core_type_eq ty_args1 ty_args2

let ty_decl_testable : type_declaration testable =
  testable pp_ty_decl ty_decl_eq

(*******************************************************************************)
(* Boilerplate for [constructor_declaration testable] *)

(** Pretty printer for the [constructor_declaration] type *)
let pp_constr_decl (ppf : Stdlib.Format.formatter)
  (cd : constructor_declaration) : unit =
  match cd.pcd_args with
  | Pcstr_tuple tys ->
    let args = String.concat ~sep:" * " (List.map ~f:string_of_core_type tys) in
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

(*******************************************************************************)
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

(*******************************************************************************)
(* Testing that monomorphization preserves core types *)

(* Strip [core_types] of location info and expose the [loc] parameter (needed
   for Ppxlib quotations) *)
let loc = Location.none

let mono_int () =
  check core_ty_testable "mono_int" (monomorphize [%type: int]) [%type: int]

let mono_string () =
  check core_ty_testable "mono_bool"
    (monomorphize [%type: string])
    [%type: string]

let mono_bool () =
  check core_ty_testable "mono_bool" (monomorphize [%type: bool]) [%type: bool]

(*******************************************************************************)
(* Monomorphization instantiates type variables with [int] *)
let mono_list () =
  check core_ty_testable "mono_list"
    (monomorphize [%type: 'a list])
    [%type: int list]

let mono_option () =
  check core_ty_testable "mono_option"
    (monomorphize [%type: 'a option])
    [%type: int option]

let mono_double_list () =
  check core_ty_testable "mono_double_list"
    (monomorphize [%type: 'a list list])
    [%type: int list list]

let mono_pair () =
  check core_ty_testable "mono_pair"
    (monomorphize [%type: 'a * 'b])
    [%type: int * int]

let mono_pair_list () =
  check core_ty_testable "mono_pair_list"
    (monomorphize [%type: ('a * 'b) list])
    [%type: (int * int) list]

let mono_func_1_arg () =
  check core_ty_testable "mono_func_1_arg"
    (monomorphize [%type: 'a -> 'b])
    [%type: int -> int]

let mono_func_2_args () =
  check core_ty_testable "mono_func_2_args"
    (monomorphize [%type: 'a -> 'b -> 'a])
    [%type: int -> int -> int]

let mono_poly_abs_type () =
  check core_ty_testable "mono_poly_abs_type"
    (monomorphize [%type: 'a t])
    [%type: int t]

let mono_qualified_poly_abs_type () =
  check core_ty_testable "mono_qualified_poly_abs_type"
    (monomorphize [%type: 'a M.t])
    [%type: int M.t]

(*******************************************************************************)
(* Testing [uniq_ret_tys] *)
let uniq_ret_tys_no_dupes () =
  let sig_items =
    [%sig:
      val x : int
      val y : string
      val z : int] in
  check core_ty_list_testable "uniq_ret_tys_no_dupes"
    (List.rev @@ uniq_ret_tys sig_items)
    [ [%type: int]; [%type: string] ]

let uniq_ret_tys_singleton () =
  let sig_items =
    [%sig:
      val x : int
      val y : int
      val z : int] in
  check core_ty_list_testable "uniq_ret_tys_singleton" (uniq_ret_tys sig_items)
    [ [%type: int] ]

let uniq_ret_tys_three_tys () =
  let sig_items =
    [%sig:
      val x : int
      val y : string
      val z : bool] in
  check core_ty_list_testable "uniq_ret_tys_three_tys"
    (List.rev @@ uniq_ret_tys sig_items)
    [ [%type: int]; [%type: string]; [%type: bool] ]

let uniq_ret_ty_1_arg_funcs () =
  let sig_items =
    [%sig:
      val f : 'a -> int
      val g : int -> string
      val h : int -> 'a] in
  check core_ty_list_testable "uniq_ret_ty_1_arg_funcs"
    (List.rev @@ uniq_ret_tys sig_items)
    [ [%type: int]; [%type: string] ]

let uniq_ret_ty_2_arg_funcs () =
  let sig_items =
    [%sig:
      val f : 'a -> int -> 'a
      val g : int -> bool -> string
      val h : bool -> char -> char] in
  check core_ty_list_testable "uniq_ret_ty_2_arg_funcs"
    (List.rev @@ uniq_ret_tys sig_items)
    [ [%type: int]; [%type: string]; [%type: char] ]

(*******************************************************************************)
(** Testing [mk_ty_constructors] *)

let mk_ty_constructors_single_base_ty () =
  let sig_items = [%sig: val x : int] in
  let expected = mk_constructor ~name:"Int" ~loc:Location.none ~arg_tys:[] in
  check constr_decl_list_testable "mk_ty_constructors_singleton"
    (mk_ty_constructors sig_items)
    [ expected ]

let mk_ty_constructors_single_mono_abs_ty () =
  let sig_items = [%sig: val x : t] in
  let expected = mk_constructor ~name:"T" ~loc:Location.none ~arg_tys:[] in
  check constr_decl_list_testable "mk_ty_constructors_single_mono_abs_ty"
    (mk_ty_constructors sig_items)
    [ expected ]

let mk_ty_constructors_single_poly_abs_ty () =
  let sig_items = [%sig: val x : 'a t] in
  let expected = mk_constructor ~name:"IntT" ~loc:Location.none ~arg_tys:[] in
  check constr_decl_list_testable "mk_ty_constructors_single_poly_abs_ty"
    (mk_ty_constructors sig_items)
    [ expected ]

let mk_ty_constructors_two_base () =
  let sig_items =
    [%sig:
      val x : int
      val y : string] in
  let expected =
    List.map
      ~f:(fun name -> mk_constructor ~name ~loc:Location.none ~arg_tys:[])
      [ "Int"; "String" ] in
  check constr_decl_list_testable "mk_ty_constructors_two"
    (mk_ty_constructors sig_items)
    expected

let mk_ty_constructors_no_dupes () =
  let sig_items =
    [%sig:
      val x : int
      val y : string
      val z : int] in
  let expected =
    List.map
      ~f:(fun name -> mk_constructor ~name ~loc:Location.none ~arg_tys:[])
      [ "Int"; "String" ] in
  check constr_decl_list_testable "mk_ty_constructors_no_dupes"
    (mk_ty_constructors sig_items)
    expected

(*******************************************************************************)
(** Testing [get_ret_ty] *)

let get_ret_ty_1_arg_func () =
  check core_ty_testable "get_ret_ty_1_arg_func"
    (get_ret_ty [%type: string -> int])
    [%type: int]

let get_ret_ty_2_arg_func () =
  check core_ty_testable "get_ret_ty_1_arg_func"
    (get_ret_ty [%type: string -> int -> bool])
    [%type: bool]

let get_ret_ty_3_arg_func () =
  check core_ty_testable "get_ret_ty_1_arg_func"
    (get_ret_ty [%type: string -> int -> bool -> char])
    [%type: char]

let get_ret_ty_uncurried () =
  check core_ty_testable "get_ret_ty_uncurried"
    (get_ret_ty [%type: string * int * bool -> char])
    [%type: char]

(*******************************************************************************)
(* TODO: - add tests for [mk_expr_constructors] *)

(*******************************************************************************)

(** Testing [string_of_lident] *)
let string_of_lident_trivial () =
  check string "string_of_lident_trivial" (string_of_lident (Lident "M")) "M"

let string_of_lident_ldot () =
  check string "string_of_lident_ldot"
    (string_of_lident (Ldot (Lident "M", "empty")))
    "M.empty"

let string_of_lident_nested_lot () =
  check string "string_of_lident_nested_lot"
    (string_of_lident (Longident.parse "M1.M2.empty"))
    "M1.M2.empty"

(*******************************************************************************)
(** Testing [uncapitalize_lident] *)

let uncapitalize_lident_trivial () =
  check lident_testable "uncapitalize_lident_trivial"
    (uncapitalize_lident (Lident "Module"))
    (Lident "module")

let uncapitalize_lident_ldot () =
  let actual = uncapitalize_lident (Longident.parse "M.Empty") in
  let expected = Longident.parse "M.empty" in
  check lident_testable "uncapitalize_lident_ldot" expected actual

let uncapitalize_lident_ldot_nested () =
  let actual = uncapitalize_lident (Longident.parse "M1.M2.Empty") in
  let expected = Longident.parse "M1.M2.empty" in
  check lident_testable "uncapitalize_lident_ldot_nested" expected actual

let uncapitalize_lident_ldot_doubly_nested () =
  let actual = uncapitalize_lident (Longident.parse "M1.M2.M3.Empty") in
  let expected = Longident.parse "M1.M2.M3.empty" in
  check lident_testable "uncapitalize_lident_ldot_nested" expected actual

(*******************************************************************************)

(** Testing [add_lident_prefix] *)
let add_lident_prefix_mod_path () =
  check lident_testable "add_lident_prefix_mod_path"
    (add_lident_prefix "M" (Lident "empty"))
    (Longident.parse "M.empty")

let add_lident_prefix_ldot () =
  check lident_testable "add_lident_prefix_ldot"
    (add_lident_prefix "M1" @@ Ldot (Lident "M2", "empty"))
    (Ldot (Lident "M1", "M2.empty"))

(*******************************************************************************)
(* Overall Alcotest Test Suite *)

let () =
  run "Utils test suite"
    [ ( "[monomorphize] preserves base types",
        [ test_case "int" `Quick mono_int;
          test_case "bool" `Quick mono_bool;
          test_case "string" `Quick mono_string
        ] );
      ( "[monomorphize] instantiates type variables with [int]",
        [ test_case "'a list" `Quick mono_list;
          test_case "'a option" `Quick mono_option;
          test_case "'a list list" `Quick mono_double_list;
          test_case "'a * 'b" `Quick mono_pair;
          test_case "('a * 'b) list" `Quick mono_pair_list;
          test_case "'a -> 'b" `Quick mono_func_1_arg;
          test_case "'a -> 'b -> 'a" `Quick mono_func_2_args;
          test_case "'a t" `Quick mono_poly_abs_type;
          test_case "'a M.t" `Quick mono_qualified_poly_abs_type
        ] );
      ( "no duplicate types in result of [uniq_ret_tys]",
        [ test_case "1 unique type" `Quick uniq_ret_tys_singleton;
          test_case "2 unique types" `Quick uniq_ret_tys_no_dupes;
          test_case "3 unique types" `Quick uniq_ret_tys_three_tys;
          test_case "1 arg function" `Quick uniq_ret_ty_1_arg_funcs;
          test_case "2 arg function" `Quick uniq_ret_ty_2_arg_funcs
        ] );
      ( "Tests for [mk_ty_constructors]",
        [ test_case "1 base type" `Quick mk_ty_constructors_single_base_ty;
          test_case "1 mono abs type" `Quick
            mk_ty_constructors_single_mono_abs_ty;
          test_case "1 poly abs type" `Quick
            mk_ty_constructors_single_mono_abs_ty;
          test_case "two constructors" `Quick mk_ty_constructors_two_base;
          test_case "no duplicates" `Quick mk_ty_constructors_two_base
        ] );
      ( "Tests for [get_ret_ty]",
        [ test_case "1 arg function" `Quick get_ret_ty_1_arg_func;
          test_case "2 arg function" `Quick get_ret_ty_2_arg_func;
          test_case "3 arg function" `Quick get_ret_ty_3_arg_func;
          test_case "uncurried function" `Quick get_ret_ty_uncurried
        ] );
      ( "Tests for [string_of_lident]",
        [ test_case "lident" `Quick string_of_lident_trivial;
          test_case "ldot" `Quick string_of_lident_ldot;
          test_case "nested ldots" `Quick string_of_lident_nested_lot
        ] );
      ( "Tests for [uncapitalize_lident]",
        [ test_case "lident" `Quick uncapitalize_lident_trivial;
          test_case "ldot" `Quick uncapitalize_lident_ldot;
          test_case "nested ldots" `Quick uncapitalize_lident_ldot_nested;
          test_case "doubly-nested ldots" `Quick
            uncapitalize_lident_ldot_doubly_nested
        ] );
      ( "Tests for [add_lident_prefix]",
        [ test_case "lident" `Quick add_lident_prefix_mod_path;
          test_case "ldot" `Quick add_lident_prefix_ldot
        ] )
    ]
