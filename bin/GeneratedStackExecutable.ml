(** Auto-generated executable for testing observational equivalence of two modules *)
(** Usage: [dune exec -- ./bin/GeneratedStackExecutable.exe] *)

(* Suppress "unused value" compiler warnings *)
[@@@ocaml.warning "-27-32-33-34"]

open Core
open Lib.Stats
open Lib.GeneratedStackPBTCode

let () = 
  let open Or_error in 
  let module QC = Quickcheck in 
  let test_bool = QC.test_or_error (gen_expr Bool) ~sexp_of:sexp_of_expr 
    ~f:(fun e ->
      match (I1.interp e, I2.interp e) with 
       | (ValBool b1, ValBool b2) ->
        try_with ~backtrace:false (fun () -> [%test_eq: bool] b1 b2)
       | v1, v2 -> error_string @@ displayError e v1 v2) in

  let test_int = QC.test_or_error (gen_expr Int) ~sexp_of:sexp_of_expr 
    ~f:(fun e ->
      match (I1.interp e, I2.interp e) with 
       | (ValInt n1, ValInt n2) ->
        try_with ~backtrace:false (fun () -> [%test_eq: int] n1 n2)
       | v1, v2 -> error_string @@ displayError e v1 v2) in

  let test_intoption = QC.test_or_error (gen_expr IntOption) ~sexp_of:sexp_of_expr 
    ~f:(fun e ->
      match (I1.interp e, I2.interp e) with 
       | (ValIntOption n1, ValIntOption n2) ->
        try_with ~backtrace:false (fun () -> [%test_eq: int option] n1 n2)
       | v1, v2 -> error_string @@ displayError e v1 v2) in

  let test_unit = QC.test_or_error (gen_expr Unit) ~sexp_of:sexp_of_expr 
    ~f:(fun e ->
      match (I1.interp e, I2.interp e) with 
       | (ValUnit u1, ValUnit u2) ->
        try_with ~backtrace:false (fun () -> [%test_eq: unit] u1 u2)
       | v1, v2 -> error_string @@ displayError e v1 v2) in

  match combine_errors_unit [
    test_bool;
    test_int;
    test_intoption;
    test_unit
  ] with 
   | Ok ok -> ok
   | Error err ->
    let open Stdlib.Format in 
    Error.pp err_formatter err;
    print_newline ()