open Base
open Yojson

module type Mica_core = sig
  type expr
  type ty

  val depth : expr -> int
  val num_unique_ints : expr -> int
  val show_expr : expr -> string
  val show_ty : ty -> string
end

module type Tyche = sig
  type expr
  type ty

  val json : Yojson.Basic.t
  val update_json : Yojson.Basic.t -> string -> Yojson.Basic.t -> Yojson.Basic.t
  val set_representation : expr -> Yojson.Basic.t -> Yojson.Basic.t
  val set_features : expr -> Yojson.Basic.t -> Yojson.Basic.t
  val add_feature : string * Yojson.Basic.t -> Yojson.Basic.t -> Yojson.Basic.t

  val set_value :
    ('a -> Yojson.Basic.t) -> 'a -> Yojson.Basic.t -> Yojson.Basic.t

  val set_start_time : Yojson.Basic.t -> Yojson.Basic.t
  val set_runtime : float -> Yojson.Basic.t -> Yojson.Basic.t
  val set_args : ty -> expr -> Yojson.Basic.t -> Yojson.Basic.t
  val set_prop : ty -> Yojson.Basic.t -> Yojson.Basic.t
  val json_pipeline : expr -> ty -> float -> Yojson.Basic.t -> Yojson.Basic.t
  val get_current_time_str : unit -> string
end

module Tyche_utils (Mica : Mica_core) :
  Tyche with type expr := Mica.expr and type ty := Mica.ty = struct
  open Mica

  (** JSON schema for Tyche visualization with initialized dummy values *)
  let json : Basic.t =
    `Assoc
      [ ("type", `String "test_case");
        ("run_start", `Null);
        ("property", `String "");
        ("status", `String "passed");
        ("status_reason", `String "");
        ("representation", `String "expr");
        ("arguments", `Assoc []);
        ("how_generated", `String "gen_expr");
        ("features", `Assoc []);
        ("metadata", `Assoc [ ("traceback", `Null) ]);
        ("coverage", `Null)
      ]

  (** [update_json json k v] updates the field [k] in the association list 
      [json] with the value [v], where [k] & [v] are both strings 
      - Raises an exception if [json] is not of the form [`Assoc fields] *)
  let update_json (json : Basic.t) (k : string) (v : Basic.t) : Basic.t =
    match json with
    | `Assoc fields ->
      let new_json = List.Assoc.add fields ~equal:String.equal k v in
      `Assoc new_json
    | _ -> failwith "Not an AssocList of json fields"

  (** Updates the [representation] field of the json with the string [s] *)
  let set_representation (e : expr) (json : Basic.t) : Basic.t =
    let expr_str = show_expr e in
    update_json json "representation" (`String expr_str)

  (** Updates the [features] field of the json to be equal to 
      [{"depth": n}] for some integer [n] *)
  let set_features (e : expr) (json : Basic.t) : Basic.t =
    update_json json "features"
    @@ `Assoc
         [ ("depth", `Int (depth e));
           ("num_unique_ints", `Int (num_unique_ints e))
           (* ("num_unique_function_calls", `Int (num_unique_constructors e)); *)
           (* ("last_function_called", `String (last_function_called e)) *)
         ]

  (** Adds an extra feature (a pair consisting of the feature name as a string
      and a [Yojson.Basic.t] value) to the existing json *)
  let add_feature (feat : string * Basic.t) (json : Basic.t) : Basic.t =
    let current = Basic.Util.member "features" json in
    update_json json "features" @@ Basic.Util.combine current (`Assoc [ feat ])

  let set_value (f : 'a -> Basic.t) (x : 'a) (json : Basic.t) : Basic.t =
    add_feature ("interpreted_value", f x) json

  (** Updates the [run_start] field of the json object
      with the current time *)
  let set_start_time (json : Basic.t) : Basic.t =
    update_json json "run_start" @@ `Float (Core_unix.time ())

  let set_runtime (runtime : float) (json : Basic.t) : Basic.t =
    update_json json "timing" @@ `Assoc [ ("execute_test", `Float runtime) ]

  (** Updates the [arguments] field of the json object with the [ty] and [expr] 
      under test *)
  let set_args (ty : ty) (e : expr) (json : Basic.t) : Basic.t =
    let expr_str = show_expr e in
    update_json json "arguments"
    @@ `Assoc [ ("ty", `String (show_ty ty)); ("expr", `String expr_str) ]

  (** Updates the [property] field of the json object with the [name] 
      of the test case and an observation type [ty] *)
  let set_prop (ty : ty) (json : Basic.t) : Basic.t =
    update_json json "property"
    @@ `String
         (Printf.sprintf "Observational Equivalence at type %s" @@ show_ty ty)

  (** End-to-end pipeline for updating the fields in the JSON object
      needed for Tyche visualization *)
  let json_pipeline (e : expr) (ty : ty) (start_time : float) (json : Basic.t) :
    Basic.t =
    let end_time = Core_unix.gettimeofday () in
    let elapsed = end_time -. start_time in
    set_start_time json |> set_args ty e |> set_prop ty |> set_features e
    |> set_representation e |> set_runtime elapsed

  (** Retrieves the current time as a formatted string *)
  let get_current_time_str () : string =
    let open Core_unix in
    gettimeofday () |> localtime |> fun tm -> strftime tm "%Y-%m-%d--%H-%M"
end
