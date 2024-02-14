(** Auto-generated executable for testing observational equivalence of two modules *)

(** Usage: [dune exec -- sets] *)

(* Suppress "unused value" compiler warnings *)
[@@@ocaml.warning "-26-27-32-33-34"]

open Core
open Lib.Stats
open Lib.GeneratedSetPBTCode
open Yojson

let json : Basic.t =
  `Assoc
    [
      ("type", `String "test_case");
      ("run_start", `Null);
      ("property", `String "Obs Equiv (Set)");
      ("status", `String "passed");
      ("status_reason", `String "");
      ("representation", `String "expr");
      ("arguments", `Assoc []);
      ("how_generated", `String "gen_expr");
      ("features", `Assoc []);
      ("metadata", `Assoc [("traceback", `Null)]);
      ("coverage", `Null);
    ]

let final_json = ref json

(** [update_json k v] updates the field [k] in the association list [json] 
    with the value [v], where [k] & [v] are both strings *)
let update_json (json : Basic.t) (k : string) (v : Basic.t) : Basic.t =
  match json with
  | `Assoc fields ->
      let new_json = List.Assoc.add fields ~equal:String.equal k v in
      `Assoc new_json
  | _ -> failwith "Not an AssocList of json fields"

(** Updates the [representation] field of the json with the string [s] *)
let set_rep_json ~rep:(e : expr) (json : Basic.t) : Basic.t =
  let expr_str = Sexp.to_string_hum @@ [%sexp_of: expr] e in  
  update_json json "representation" (`String expr_str)

(** Updates the [features] field of the json to be equal to 
    [{"depth": n}] for some integer [n] *)
let set_depth_json ~depth:(n : int)  (json : Basic.t) : Basic.t =
  update_json json "features" (`Assoc [ ("depth", `Int n) ])

(** Updates the [run_start] field of the json object
    with the current time *)  
let set_start_time_json (json : Basic.t) () : Basic.t = 
  update_json json "run_start" @@ `Float (Core_unix.time ())

let set_runtime_json (runtime : float) (json : Basic.t) : Basic.t = 
  update_json json "timing" @@ `Assoc [("execute_test", `Float runtime)]  

(** Updates the [arguments] field of the json object *)
let set_args_json (e : expr) (json : Basic.t) : Basic.t =
  let expr_str = Sexp.to_string_hum @@ [%sexp_of: expr] e in 
  update_json json "arguments" @@ `Assoc [("expr", `String expr_str)]

(** Appends a JSON object containing metadata for the expr [e] 
    to the list [jsons] *)
(* let append_to_jsons (e : expr) : unit =
  let sexp_str = Sexp.to_string (sexp_of_expr e) in
  final_json := set_args_json sexp_str (depth e) json *)

let json_pipeline (e : expr) (start_time : float) (json : Basic.t) : Basic.t = 
  let end_time = Core_unix.gettimeofday () in 
  let elapsed = end_time -. start_time in 
  let depth = depth e in 
  (* TODO: need to update the property field with the type we're checking OE at *)
  set_args_json e json
    |> set_depth_json ~depth
    |> set_rep_json ~rep:e
    |> set_runtime_json elapsed

let () =
  let json_log = Out_channel.create ~append:false "feb_3_testcases.jsonl" in

  let json_seq_ref = ref Seq.empty in 

  let open Or_error in
  let module QC = Quickcheck in
  let module G = QC.Generator in
  let seed = `Nondeterministic in
  let trials = 10 in
  let sexp_of = sexp_of_expr in
  let test_bool =
    (* Note that we initialize [gen_expr] with the empty context *)
    QC.test_or_error
      (gen_expr [] Bool)
      ~seed ~trials ~sexp_of
      ~f:(fun e ->
        let start_time = Core_unix.gettimeofday () in
        let depth = depth e in 
        match (I1.interp e, I2.interp e) with
        | ValBool b1, ValBool b2 ->
            try_with ~backtrace:false (fun () -> 
              let end_time = Core_unix.gettimeofday () in 
              let elapsed = end_time -. start_time in
              let j1 = set_runtime_json elapsed json in  
              let final_json = failwith "TODO" in 
              json_seq_ref := Seq.cons final_json !json_seq_ref;
              [%test_eq: bool] b1 b2)
        | v1, v2 -> error_string @@ displayError e v1 v2)
  in

  let test_int =
    QC.test_or_error
      (gen_expr [] Int)
      ~seed ~trials ~sexp_of
      ~f:(fun e ->
        match (I1.interp e, I2.interp e) with
        | ValInt n1, ValInt n2 ->
            try_with ~backtrace:false (fun () -> [%test_eq: int] n1 n2)
        | v1, v2 -> error_string @@ displayError e v1 v2)
  in
  Basic.to_channel json_log !final_json;
  Out_channel.close json_log;
  match combine_errors_unit [ test_bool; test_int ] with
  | Ok ok -> printf "Test succeeded\n"
  | Error err ->
      let open Stdlib.Format in
      Error.pp err_formatter err
