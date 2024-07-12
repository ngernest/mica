open Ppx_mica__Utils
open Alcotest
open Boilerplate

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
