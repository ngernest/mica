open Ppx_mica__Utils
open Boilerplate

(******************************************************************************)
(* Testing that monomorphization preserves core types *)

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
