open Ppx_mica__Type_deriver
open Ppx_mica__Utils
open Alcotest
open Boilerplate

let loc = Location.none

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
(** Testing [get_arg_tys_of_expr_cstr] *)

let get_arg_tys_of_expr_cstr_hof () =
  let expected = [ [%type: int -> int]; [%type: expr] ] in
  let actual = get_arg_tys_of_expr_cstr [%type: ('a -> 'b) -> 'a t] [ "t" ] in
  mk_test core_ty_list_testable "('a -> 'b) -> 'a t" expected actual

let get_arg_tys_of_expr_cstr_map () =
  let expected = [ [%type: int -> int]; [%type: expr]; [%type: expr] ] in
  let actual =
    get_arg_tys_of_expr_cstr [%type: ('a -> 'b) -> 'a t -> 'b t] [ "t" ] in
  mk_test core_ty_list_testable "map : ('a -> 'b) -> 'a t -> 'b t" expected
    actual
