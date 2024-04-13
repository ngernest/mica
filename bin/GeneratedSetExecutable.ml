(** Auto-generated executable for testing observational equivalence of two modules *)

(** Usage: [dune exec -- sets] *)

(* Suppress "unused value" compiler warnings *)
[@@@ocaml.warning "-26-27-32-33-34"]

open Core
open Lib.Stats
open Lib.GeneratedSetPBTCode
open Lib.JsonUtils
module QC = Quickcheck
module G = QC.Generator

let () =
  let open Or_error in
  let json_seq_ref = ref Seq.empty in
  let seed = `Nondeterministic in
  let trials = 100 in
  let sexp_of = sexp_of_expr in
  let test_bool =
    (* Note that we initialize [gen_expr] with the empty context *)
    QC.test_or_error (gen_expr [] Bool) ~seed ~trials ~sexp_of ~f:(fun e ->
        let start_time = Core_unix.gettimeofday () in
        match (I1.interp e, I2.interp e) with
        | ValBool b1, ValBool b2 ->
            try_with ~backtrace:false (fun () ->
                let final_json = json_pipeline e Bool start_time json in
                json_seq_ref := Seq.cons final_json !json_seq_ref;
                [%test_eq: bool] b1 b2)
        | v1, v2 -> error_string @@ displayError e v1 v2)
  in

  let test_int =
    QC.test_or_error (gen_expr [] Int) ~seed ~trials ~sexp_of ~f:(fun e ->
        let start_time = Core_unix.gettimeofday () in
        match (I1.interp e, I2.interp e) with
        | ValInt n1, ValInt n2 ->
            try_with ~backtrace:false (fun () ->
                let final_json = json_pipeline e Int start_time json in
                json_seq_ref := Seq.cons final_json !json_seq_ref;
                [%test_eq: int] n1 n2)
        | v1, v2 -> error_string @@ displayError e v1 v2)
  in
  Yojson.Basic.seq_to_file "feb_14_testcases.jsonl" !json_seq_ref;
  match combine_errors_unit [ test_bool; test_int ] with
  | Ok ok -> printf "Test succeeded\n"
  | Error err ->
      let open Stdlib.Format in
      Error.pp err_formatter err
