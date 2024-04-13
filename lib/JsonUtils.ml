open Core
open Yojson

(* TODO: figure out a better way of injecting the dependency on
   the [expr] type declaration *)
open GeneratedSetPBTCode

(** JSON schema for Tyche visualization with initialized dummy values *)
let json : Basic.t =
  `Assoc
    [
      ("type", `String "test_case");
      ("run_start", `Null);
      ("property", `String "");
      ("status", `String "passed");
      ("status_reason", `String "");
      ("representation", `String "expr");
      ("arguments", `Assoc []);
      ("how_generated", `String "gen_expr");
      ("features", `Assoc []);
      ("metadata", `Assoc [ ("traceback", `Null) ]);
      ("coverage", `Null);
    ]

(** [update_json k v] updates the field [k] in the association list [json] 
    with the value [v], where [k] & [v] are both strings *)
let update_json (json : Basic.t) (k : string) (v : Basic.t) : Basic.t =
  match json with
  | `Assoc fields ->
      let new_json = List.Assoc.add fields ~equal:String.equal k v in
      `Assoc new_json
  | _ -> failwith "Not an AssocList of json fields"

(** Updates the [representation] field of the json with the string [s] *)
let set_representation (e : expr) (json : Basic.t) : Basic.t =
  let expr_str = Sexp.to_string_hum @@ [%sexp_of: expr] e in
  update_json json "representation" (`String expr_str)

(** Updates the [features] field of the json to be equal to 
    [{"depth": n}] for some integer [n] *)
let set_depth (e : expr) (json : Basic.t) : Basic.t =
  update_json json "features"
  @@ `Assoc
       [
         ("depth", `Int (depth e)); ("num_unique_ints", `Int (num_unique_ints e));
       ]

(** Updates the [run_start] field of the json object
    with the current time *)
let set_start_time (json : Basic.t) : Basic.t =
  update_json json "run_start" @@ `Float (Core_unix.time ())

let set_runtime (runtime : float) (json : Basic.t) : Basic.t =
  update_json json "timing" @@ `Assoc [ ("execute_test", `Float runtime) ]

(** Updates the [arguments] field of the json object *)
let set_args (e : expr) (json : Basic.t) : Basic.t =
  let expr_str = Sexp.to_string_hum @@ [%sexp_of: expr] e in
  update_json json "arguments" @@ `Assoc [ ("expr", `String expr_str) ]

(** Updates the [property] field of the json object *)
let set_prop (ty : ty) (json : Basic.t) : Basic.t =
  let ty_str = Sexp.to_string_hum @@ [%sexp_of: ty] ty in
  update_json json "property"
  @@ `String
       (Printf.sprintf
          "Observational Equivalence for Set implementations at type %s" ty_str)

(** End-to-end pipeline for updating the fields in the JSON object
    needed for Tyche visualization *)
let json_pipeline (e : expr) (ty : ty) (start_time : float) (json : Basic.t) :
    Basic.t =
  let end_time = Core_unix.gettimeofday () in
  let elapsed = end_time -. start_time in
  set_start_time json |> set_args e |> set_prop ty |> set_depth e
  |> set_representation e |> set_runtime elapsed
