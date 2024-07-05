open Ppx_mica__Utils
open Ppx_mica__Type_deriver
open Ppxlib
open Astlib.Pprintast
open StdLabels
open Alcotest

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
(* Boilerplate for [type_declaration testable] (currently unused) *)

(* let pp_ty_decl = Ppxlib.Pprintast.type_declaration

   (** Equality of [type_declaration]'s is based on their string representations
   *) let ty_decl_eq ({ ptype_name = ty1; ptype_params = ty_params1; _ } :
   type_declaration) ({ ptype_name = ty2; ptype_params = ty_params2; _ } :
   type_declaration) : bool = let ty_args1, ty_args2 = map2 ~f:(List.map ~f:fst)
   (ty_params1, ty_params2) in String.equal (no_loc ty1) (no_loc ty2) &&
   List.equal ~eq:core_type_eq ty_args1 ty_args2

   let ty_decl_testable : type_declaration testable = testable pp_ty_decl
   ty_decl_eq *)

(******************************************************************************)
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

(******************************************************************************)
(* Testing that monomorphization preserves core types *)

(* Strip [core_types] of location info and expose the [loc] parameter (needed
   for Ppxlib quotations) *)
let loc = Location.none

let mono_int () =
  mk_test core_ty_testable "int" (monomorphize [%type: int]) [%type: int]

let mono_string () =
  mk_test core_ty_testable "string"
    (monomorphize [%type: string])
    [%type: string]

let mono_bool () =
  mk_test core_ty_testable "bool" (monomorphize [%type: bool]) [%type: bool]

(******************************************************************************)
(* Monomorphization instantiates type variables with [int] *)
let mono_list () =
  mk_test core_ty_testable "'a list"
    (monomorphize [%type: 'a list])
    [%type: int list]

let mono_option () =
  mk_test core_ty_testable "'a option"
    (monomorphize [%type: 'a option])
    [%type: int option]

let mono_double_list () =
  mk_test core_ty_testable "'a list list"
    (monomorphize [%type: 'a list list])
    [%type: int list list]

let mono_pair () =
  mk_test core_ty_testable "'a * 'b"
    (monomorphize [%type: 'a * 'b])
    [%type: int * int]

let mono_pair_list () =
  mk_test core_ty_testable "('a * 'b) list"
    (monomorphize [%type: ('a * 'b) list])
    [%type: (int * int) list]

let mono_func_1_arg () =
  mk_test core_ty_testable "'a -> 'b"
    (monomorphize [%type: 'a -> 'b])
    [%type: int -> int]

let mono_func_2_args () =
  mk_test core_ty_testable "'a -> 'b -> 'a"
    (monomorphize [%type: 'a -> 'b -> 'a])
    [%type: int -> int -> int]

let mono_poly_abs_type () =
  mk_test core_ty_testable "'a t" (monomorphize [%type: 'a t]) [%type: int t]

let mono_qualified_poly_abs_type () =
  mk_test core_ty_testable "'a M.t"
    (monomorphize [%type: 'a M.t])
    [%type: int M.t]

(******************************************************************************)
(* Testing [uniq_ret_tys] *)
let uniq_ret_tys_no_dupes () =
  let sig_items =
    [%sig:
      val x : int
      val y : string
      val z : int] in
  mk_test core_ty_list_testable "1 unique type" (uniq_ret_tys sig_items)
    [ [%type: int]; [%type: string] ]

let uniq_ret_tys_singleton () =
  let sig_items =
    [%sig:
      val x : int
      val y : int
      val z : int] in
  mk_test core_ty_list_testable "2 unique types" (uniq_ret_tys sig_items)
    [ [%type: int] ]

let uniq_ret_tys_three_tys () =
  let sig_items =
    [%sig:
      val x : int
      val y : string
      val z : bool] in
  mk_test core_ty_list_testable "3 unique types" (uniq_ret_tys sig_items)
    [ [%type: int]; [%type: string]; [%type: bool] ]

let uniq_ret_ty_1_arg_funcs () =
  let sig_items =
    [%sig:
      val f : 'a -> int
      val g : int -> string
      val h : int -> 'a] in
  mk_test core_ty_list_testable "unary function" (uniq_ret_tys sig_items)
    [ [%type: int]; [%type: string] ]

let uniq_ret_ty_2_arg_funcs () =
  let sig_items =
    [%sig:
      val f : 'a -> int -> 'a
      val g : int -> bool -> string
      val h : bool -> char -> char] in
  mk_test core_ty_list_testable "binary function" (uniq_ret_tys sig_items)
    [ [%type: int]; [%type: string]; [%type: char] ]

(******************************************************************************)
(** Tests for [mk_ty_cstrs] *)

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
(** Testing [get_ret_ty] *)

let get_ret_ty_1_arg_func () =
  mk_test core_ty_testable "unary function"
    (get_ret_ty [%type: string -> int])
    [%type: int]

let get_ret_ty_2_arg_func () =
  mk_test core_ty_testable "binary function"
    (get_ret_ty [%type: string -> int -> bool])
    [%type: bool]

let get_ret_ty_3_arg_func () =
  mk_test core_ty_testable "ternary function"
    (get_ret_ty [%type: string -> int -> bool -> char])
    [%type: char]

let get_ret_ty_uncurried () =
  mk_test core_ty_testable "get_ret_ty_uncurried"
    (get_ret_ty [%type: string * int * bool -> char])
    [%type: char]

(******************************************************************************)
(* TODO: - add tests for [mk_expr_cstrs] *)

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

(******************************************************************************)
(** Testing [is_abs_ty_parameterized] *)

let is_abs_ty_parameterized_empty_sig () =
  mk_test bool "empty signature" (is_abs_ty_parameterized [%sig:]) false

let is_abs_ty_parameterized_sig_no_abs_ty () =
  mk_test bool "no abstract types"
    (is_abs_ty_parameterized
       [%sig:
         val f : int -> int
         val g : int -> bool -> int])
    false

let is_abs_ty_parameterized_t () =
  mk_test bool "t"
    (is_abs_ty_parameterized
       [%sig:
         type t

         val f : t -> int
         val g : string -> t])
    false

let is_abs_ty_parameterized_alpha_t () =
  mk_test bool "'a t"
    (is_abs_ty_parameterized
       [%sig:
         type 'a t

         val f : 'a t -> 'a
         val g : 'a -> 'a t])
    true

let is_abs_ty_parameterized_alpha_beta_t () =
  mk_test bool "('a, 'b) t"
    (is_abs_ty_parameterized
       [%sig:
         type ('a, 'b) t

         val f : ('a, 'b) t -> 'a
         val g : ('a, 'b) t -> 'b])
    true

(******************************************************************************)
(** Testing [update_expr_arg_names] *)

let update_expr_arg_names_singleton () =
  mk_test (list string) "singleton"
    (update_expr_arg_names [ "x'" ] [ "x" ])
    [ "x'" ]

let update_expr_arg_names_no_op () =
  mk_test (list string) "no-op"
    (update_expr_arg_names [ "x'" ] [ "x1"; "x2"; "x3" ])
    [ "x1"; "x2"; "x3" ]

let update_expr_arg_names_update_one () =
  mk_test (list string) "update 1"
    (update_expr_arg_names [ "x2'" ] [ "x1"; "x2"; "x3" ])
    [ "x1"; "x2'"; "x3" ]

let update_expr_arg_names_update_two () =
  mk_test (list string) "update 2"
    (update_expr_arg_names [ "x2'"; "x4'" ] [ "x1"; "x2"; "x3"; "x4"; "x5" ])
    [ "x1"; "x2'"; "x3"; "x4'"; "x5" ]

let update_expr_arg_names_double_primes () =
  mk_test (list string) "double primes"
    (update_expr_arg_names [ "x''"; "y''" ] [ "w'"; "x'"; "y'"; "z'" ])
    [ "w'"; "x''"; "y''"; "z'" ]

(******************************************************************************)
(** Testing [get_ty_decls_from_sig] *)

(** [testable] instance for the type [(string * core_type) list list] *)
let string_core_ty_list_list = list (pair string (list core_ty_testable))

let get_ty_decls_from_sig_t () =
  mk_test string_core_ty_list_list "t"
    (get_ty_decls_from_sig [%sig: type t])
    [ ("t", []) ]

let get_ty_decls_from_sig_t_int () =
  mk_test string_core_ty_list_list "t = int"
    (get_ty_decls_from_sig [%sig: type t = int])
    [ ("t", []) ]

let get_ty_decls_from_sig_alpha_t () =
  mk_test string_core_ty_list_list "'a t"
    (get_ty_decls_from_sig [%sig: type 'a t])
    [ ("t", [ [%type: 'a] ]) ]

let get_ty_decls_from_sig_alpha_beta_t () =
  mk_test string_core_ty_list_list "('a, 'b) t"
    (get_ty_decls_from_sig [%sig: type ('a, 'b) t])
    [ ("t", [ [%type: 'a]; [%type: 'b] ]) ]

let get_ty_decls_from_sig_alpha_beta_gamma_t () =
  mk_test string_core_ty_list_list "('a, 'b, 'c) t"
    (get_ty_decls_from_sig [%sig: type ('a, 'b, 'c) t])
    [ ("t", [ [%type: 'a]; [%type: 'b]; [%type: 'c] ]) ]

let get_ty_decls_from_sig_ignore_vals_alpha_t () =
  mk_test string_core_ty_list_list "'a t, ignore vals'"
    (get_ty_decls_from_sig
       [%sig:
         val f : 'a -> 'a
         val g : int -> int

         type 'a t])
    [ ("t", [ [%type: 'a] ]) ]

let get_ty_decls_from_sig_two_tys () =
  mk_test string_core_ty_list_list "'a t; 'b u"
    (get_ty_decls_from_sig
       [%sig:
         type 'a t
         type 'b u])
    [ ("t", [ [%type: 'a] ]); ("u", [ [%type: 'b] ]) ]

let get_ty_decls_from_sig_three_tys () =
  mk_test string_core_ty_list_list "'a t; 'b u; ('c, 'c) v"
    (get_ty_decls_from_sig
       [%sig:
         type 'a t
         type 'b u
         type ('c, 'd) v])
    [ ("t", [ [%type: 'a] ]);
      ("u", [ [%type: 'b] ]);
      ("v", [ [%type: 'c]; [%type: 'd] ])
    ]

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

(******************************************************************************)
(* Tests for [get_abs_ty_names] *)

let get_abs_ty_names_empty () =
  mk_test (list string) "empty sig" (get_abs_ty_names [%sig:]) []

let get_abs_ty_names_t () =
  mk_test (list string) "t" (get_abs_ty_names [%sig: type t]) [ "t" ]

let get_abs_ty_names_alpha_t () =
  mk_test (list string) "'a t" (get_abs_ty_names [%sig: type 'a t]) [ "t" ]

let get_abs_ty_names_two_types () =
  mk_test (list string) "type alpha; type beta"
    (get_abs_ty_names
       [%sig:
         type alpha
         type beta])
    [ "alpha"; "beta" ]

let get_abs_ty_names_three_types () =
  mk_test (list string) "type alpha; type beta"
    (get_abs_ty_names
       [%sig:
         type t

         val f : int -> int

         type u

         val g : bool -> int

         type v])
    [ "t"; "u"; "v" ]

(******************************************************************************)
(* Tests for [gen_atom] *)
let gen_atom_int () = 
  let expected = "quickcheck_generator_int" in 
  let actual = gen_atom ~loc [%type: int] |> string_of_expression in
  mk_test string "int" expected actual 

let gen_atom_char () = 
  let expected = "quickcheck_generator_char" in 
  let actual = gen_atom ~loc [%type: char] |> string_of_expression in
  mk_test string "char" expected actual   

let gen_atom_string () = 
  let expected = "quickcheck_generator_string" in 
  let actual = gen_atom ~loc [%type: string] |> string_of_expression in
  mk_test string "string" expected actual 

let gen_atom_int_list () = 
  let expected = "quickcheck_generator_list quickcheck_generator_int" in 
  let actual = gen_atom ~loc [%type: int list] |> string_of_expression in 
  mk_test string "int list" expected actual

let gen_atom_char_option () = 
  let expected = "quickcheck_generator_option quickcheck_generator_char" in 
  let actual = gen_atom ~loc [%type: char option] |> string_of_expression in 
  mk_test string "char option" expected actual  

(******************************************************************************)
(* Overall Alcotest Test Suite *)

let () =
  run "Utils test suite"
    [ ( "monomorphize (base types)",
        [ mono_int (); mono_string (); mono_bool () ] );
      ( "monomorphize (instantiate type variables)",
        [ mono_list ();
          mono_option ();
          mono_double_list ();
          mono_pair ();
          mono_pair_list ();
          mono_func_1_arg ();
          mono_func_2_args ();
          mono_poly_abs_type ();
          mono_qualified_poly_abs_type ()
        ] );
      ( "uniq_ret_tys",
        [ uniq_ret_tys_singleton ();
          uniq_ret_tys_no_dupes ();
          uniq_ret_tys_three_tys ();
          uniq_ret_ty_1_arg_funcs ();
          uniq_ret_ty_2_arg_funcs ()
        ] );
      ( "mk_ty_cstrs",
        [ mk_ty_cstrs_single_base_ty ();
          mk_ty_cstrs_single_mono_abs_ty ();
          mk_ty_cstrs_single_poly_abs_ty ();
          mk_ty_cstrs_two_base ();
          mk_ty_cstrs_no_dupes ()
        ] );
      ( "get_ret_ty",
        [ get_ret_ty_1_arg_func ();
          get_ret_ty_2_arg_func ();
          get_ret_ty_3_arg_func ();
          get_ret_ty_uncurried ()
        ] );
      ( "string_of_lident",
        [ string_of_lident_trivial ();
          string_of_lident_ldot ();
          string_of_lident_nested_lot ()
        ] );
      ( "uncapitalize_lident",
        [ uncapitalize_lident_trivial ();
          uncapitalize_lident_ldot ();
          uncapitalize_lident_ldot_nested ();
          uncapitalize_lident_ldot_doubly_nested ()
        ] );
      ( "add_lident_prefix",
        [ add_lident_prefix_mod_path (); add_lident_prefix_ldot () ] );
      ( "is_abs_ty_parameterized",
        [ is_abs_ty_parameterized_empty_sig ();
          is_abs_ty_parameterized_sig_no_abs_ty ();
          is_abs_ty_parameterized_t ();
          is_abs_ty_parameterized_alpha_t ();
          is_abs_ty_parameterized_alpha_beta_t ()
        ] );
      ( "update_expr_arg_names",
        [ update_expr_arg_names_singleton ();
          update_expr_arg_names_no_op ();
          update_expr_arg_names_update_one ();
          update_expr_arg_names_update_two ();
          update_expr_arg_names_double_primes ()
        ] );
      ( "get_ty_decls_from_sig",
        [ get_ty_decls_from_sig_t ();
          get_ty_decls_from_sig_t_int ();
          get_ty_decls_from_sig_alpha_t ();
          get_ty_decls_from_sig_alpha_beta_t ();
          get_ty_decls_from_sig_alpha_beta_gamma_t ();
          get_ty_decls_from_sig_ignore_vals_alpha_t ();
          get_ty_decls_from_sig_two_tys ();
          get_ty_decls_from_sig_three_tys ()
        ] );
      ( "equal_core_type_ty_cstr",
        [ equal_core_ty_ty_cstr_bool_Bool ();
          equal_core_ty_ty_cstr_int_Int ();
          equal_core_ty_ty_cstr_t_T ();
          equal_core_ty_ty_cstr_alpha_Int ();
          equal_core_ty_ty_cstr_alpha_t_IntT ();
          equal_core_ty_ty_cstr_alpha_int_list_IntList ();
          equal_core_ty_ty_cstr_alpha_string_option_StringOption ();
          equal_core_ty_ty_cstr_product_type ();
          equal_core_type_ty_cstr_function_type ()
        ] );
      ( "equal_core_type",
        [ equal_core_type_any_refl ();
          equal_core_type_int_refl ();
          equal_core_type_int_bool_neq ();
          equal_core_type_int_string_product_refl ();
          equal_core_type_int_string_product_permute ();
          equal_core_type_string_list_refl ();
          equal_core_type_different_list_types ();
          equal_core_type_nested_option_list_refl ();
          equal_core_type_function_types_refl ();
          equal_core_type_alpha_t_refl ();
          equal_core_type_alpha_beta_t_neq ()
        ] );
      ( "equal_constructor_declaration",
        [ equal_constructor_declaration_enum_refl ();
          equal_constructor_declaration_unary_refl ();
          equal_constructor_declaration_unary_diff_names ();
          equal_constructor_declaration_unary_same_name_diff_arg_types ();
          equal_constructor_declaration_binary_refl ();
          equal_constructor_declaration_binary_diff_names ();
          equal_constructor_declaration_binary_permute_args ()
        ] );
      ( "get_abs_ty_names",
        [ get_abs_ty_names_empty ();
          get_abs_ty_names_t ();
          get_abs_ty_names_alpha_t ();
          get_abs_ty_names_two_types ();
          get_abs_ty_names_three_types ()
        ] );
      ( "get_cstr_arity",
        [ get_cstr_arity_nullary ();
          get_cstr_arity_unary ();
          get_cstr_arity_binary ();
          get_cstr_arity_ternary ()
        ] );
      ( "gen_atom", 
        [ gen_atom_int ();
          gen_atom_char ();
          gen_atom_string ();
          gen_atom_int_list ();
          gen_atom_char_option ();
        ])
    ]
