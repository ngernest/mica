open Ppx_mica__Utils
open Ppx_mica__Test_harness_deriver
open Alcotest
open Boilerplate

let loc = Location.none

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
(* Tests for [get_abs_ty_names] *)

let get_abs_ty_names_empty () =
  mk_test (list string) "empty sig" (get_abs_ty_names [%sig:]) []

let get_abs_ty_names_t () =
  mk_test (list string) "t" (get_abs_ty_names [%sig: type t]) [ "t" ]

let get_abs_ty_names_alpha_t () =
  mk_test (list string) "'a t" (get_abs_ty_names [%sig: type 'a t]) [ "t" ]

let get_abs_ty_names_alpha_beta_t () =
  mk_test (list string) "('a, 'b) t"
    (get_abs_ty_names [%sig: type ('a, 'b) t])
    [ "t" ]

let get_abs_ty_names_set_abstract_type () =
  mk_test (list string) "type set"
    (get_abs_ty_names
       [%sig:
         type 'a set

         val of_list : 'a list -> 'a set])
    [ "set" ]

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
(* Tests for [get_abs_ty_names] *)

let check_type_is_concrete_t_int_true () =
  mk_test bool "int != t" (check_type_is_concrete [ "t" ] [%type: int]) true

let check_type_is_concrete_t_string_option_true () =
  mk_test bool "int != t"
    (check_type_is_concrete [ "t" ] [%type: string option])
    true

let check_type_is_concrete_t_int_bool_true () =
  mk_test bool "int != t"
    (check_type_is_concrete [ "t" ] [%type: int -> bool])
    true

let check_type_is_concrete_t_t_false () =
  mk_test bool "t == t" (check_type_is_concrete [ "t" ] [%type: t]) false

let check_type_is_concrete_t_int_t_false () =
  mk_test bool "t == t" (check_type_is_concrete [ "t" ] [%type: int t]) false

let check_type_is_concrete_t_alpha_t_false () =
  mk_test bool "t == t" (check_type_is_concrete [ "t" ] [%type: 'a t]) false

let check_type_is_concrete_t_t_option_false () =
  mk_test bool "t == t" (check_type_is_concrete [ "t" ] [%type: t option]) false

let check_type_is_concrete_t_int_t_list_false () =
  mk_test bool "t == t"
    (check_type_is_concrete [ "t" ] [%type: int t list])
    false

let check_type_is_concrete_t_func_arg_false () =
  mk_test bool "t == t" (check_type_is_concrete [ "t" ] [%type: t -> int]) false

let check_type_is_concrete_t_2nd_func_arg_false () =
  mk_test bool "t == t"
    (check_type_is_concrete [ "t" ] [%type: int -> t -> bool])
    false

let check_type_is_concrete_t_func_res_false () =
  mk_test bool "t == t" (check_type_is_concrete [ "t" ] [%type: int -> t]) false

let check_type_is_concrete_t_prod_type_false () =
  mk_test bool "t == t" (check_type_is_concrete [ "t" ] [%type: int * t]) false
