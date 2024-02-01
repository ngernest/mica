(** Auto-generated executable for testing observational equivalence of two modules *)

(** Usage: [dune exec -- sets] *)

(* Suppress "unused value" compiler warnings *)
[@@@ocaml.warning "-26-27-32-33-34"]

open Core
open Lib.Stats
open Lib.GeneratedSetPBTCode
open Yojson


let json : Yojson.Basic.t = `Assoc [
  ("type", `String "test_case");
  ("status", `String "passed");
  ("status_reason", `String "");
  ("representation", `String "TODO")
  (* TODO: continue populating json *)
]

(** [update_json k v] updates the field [k] in the association list [json] 
    with the value [v], where [k] & [v] are both strings *)
let update_json (`Assoc json : Yojson.Basic.t) 
                 (k : string) (v: string) : Yojson.Basic.t = 
  let new_json = List.Assoc.add json ~equal:String.equal k (`String v) in 
  `Assoc new_json


let () =
  let logFile = Out_channel.create ~append:false "log.txt" in

  let open Or_error in
  let module QC = Quickcheck in
  let module G = QC.Generator in
  let seed = `Nondeterministic in
  let trials = 7 in
  let sexp_of = sexp_of_expr in
  let test_bool =
    (* Note that we initialize [gen_expr] with the empty context *)
    QC.test_or_error
      (G.filter ~f:not_trivial @@ gen_expr [] Bool)
      ~seed ~trials ~sexp_of
      ~f:(fun e ->
        print_s (sexp_of_expr e);

        
        match (I1.interp e, I2.interp e) with
        | ValBool b1, ValBool b2 ->
            try_with ~backtrace:false (fun () -> [%test_eq: bool] b1 b2)
        | v1, v2 -> error_string @@ displayError e v1 v2)
  in

  let test_int =
    QC.test_or_error
      (G.filter ~f:not_trivial @@ gen_expr [] Int)
      ~seed ~trials ~sexp_of
      ~f:(fun e ->
        print_s (sexp_of_expr e);
        match (I1.interp e, I2.interp e) with
        | ValInt n1, ValInt n2 ->
            try_with ~backtrace:false (fun () -> [%test_eq: int] n1 n2)
        | v1, v2 -> error_string @@ displayError e v1 v2)
  in

  match combine_errors_unit [ test_bool; test_int ] with
  | Ok ok ->
      printf "\nTest succeeded\n";
      Out_channel.close logFile

      
  | Error err ->
      let open Stdlib.Format in
      Error.pp err_formatter err;
      print_newline ()
