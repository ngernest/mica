(** Auto-generated executable for testing observational equivalence of two modules *)

(** Usage: [dune exec -- ./bin/GeneratedMapExecutable.exe] *)

(* Suppress "unused value" compiler warnings *)
[@@@ocaml.warning "-27-32-33-34"]

open Core
open Lib.Stats
open Lib.GeneratedMapPBTCode

let () =
  let open Or_error in
  let module QC = Quickcheck in
  let test_stringoption =
    QC.test_or_error (gen_expr StringOption) ~sexp_of:sexp_of_expr ~f:(fun e ->
      match (I1.interp e, I2.interp e) with
      | ValStringOption s1, ValStringOption s2 ->
        try_with ~backtrace:false (fun () -> [%test_eq: string option] s1 s2)
      | v1, v2 -> error_string @@ displayError e v1 v2) in

  match combine_errors_unit [ test_stringoption ] with
  | Ok ok ->
    let numPassed = QC.default_can_generate_trial_count in
    let numDiscarded = QC.(default_trial_count - numPassed) in
    printf "\n Mica: OK, passed %d tests; %d discarded. \n" numPassed
      numDiscarded
  | Error err ->
    let open Stdlib.Format in
    Error.pp err_formatter err;
    print_newline ()
