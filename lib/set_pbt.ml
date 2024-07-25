module type S = sig
  type 'a t

  val empty : 'a t
  val is_empty : 'a t -> bool
  val mem : 'a -> 'a t -> bool
  val add : 'a -> 'a t -> 'a t
  val rem : 'a -> 'a t -> 'a t
  val size : 'a t -> int
  val union : 'a t -> 'a t -> 'a t
  val intersect : 'a t -> 'a t -> 'a t
  val invariant : 'a t -> bool
end

(* Suppress "unused value" compiler warnings *)
[@@@ocaml.warning "-27-32-33-34"]

type expr =
  | Empty
  | Is_empty of expr
  | Mem of int * expr
  | Add of int * expr
  | Rem of int * expr
  | Size of expr
  | Union of expr * expr
  | Intersect of expr * expr
  | Invariant of expr

type ty = Bool | Int | IntT

let rec gen_expr ty =
  let open Core in
  let open Quickcheck.Generator in
  let open Let_syntax in
  size >>= fun k ->
  match ty with
  | Bool ->
    let gen_is_empty =
      let g__001_ = with_size ~size:(k / 2) (gen_expr IntT) in
      g__001_ >>| fun e__002_ -> Is_empty e__002_
    and gen_mem =
      let g__003_ = quickcheck_generator_int
      and g__004_ = with_size ~size:(k / 2) (gen_expr IntT) in
      tuple2 g__003_ g__004_ >>| fun (e__005_, e__006_) -> Mem (e__005_, e__006_)
    and gen_invariant =
      let g__007_ = with_size ~size:(k / 2) (gen_expr IntT) in
      g__007_ >>| fun e__008_ -> Invariant e__008_ in
    union [ gen_is_empty; gen_mem; gen_invariant ]
  | Int ->
    let gen_size =
      let g__009_ = with_size ~size:(k / 2) (gen_expr IntT) in
      g__009_ >>| fun e__010_ -> Size e__010_ in
    union [ gen_size ]
  | IntT ->
    let gen_empty = return Empty
    and gen_add =
      let g__011_ = quickcheck_generator_int
      and g__012_ = with_size ~size:(k / 2) (gen_expr IntT) in
      tuple2 g__011_ g__012_ >>| fun (e__013_, e__014_) -> Add (e__013_, e__014_)
    and gen_rem =
      let g__015_ = quickcheck_generator_int
      and g__016_ = with_size ~size:(k / 2) (gen_expr IntT) in
      tuple2 g__015_ g__016_ >>| fun (e__017_, e__018_) -> Rem (e__017_, e__018_)
    and gen_union =
      let g__019_ = with_size ~size:(k / 2) (gen_expr IntT)
      and g__020_ = with_size ~size:(k / 2) (gen_expr IntT) in
      tuple2 g__019_ g__020_ >>| fun (e__021_, e__022_) ->
      Union (e__021_, e__022_)
    and gen_intersect =
      let g__023_ = with_size ~size:(k / 2) (gen_expr IntT)
      and g__024_ = with_size ~size:(k / 2) (gen_expr IntT) in
      tuple2 g__023_ g__024_ >>| fun (e__025_, e__026_) ->
      Intersect (e__025_, e__026_) in
    union [ gen_empty; gen_add; gen_rem; gen_union; gen_intersect ]

let _ = gen_expr

module Interpret (M : S) = struct
  include M

  type value = ValBool of bool | ValInt of int | ValIntT of int t

  let rec interp e =
    match e with
    | Empty -> ValIntT M.empty
    | Is_empty expr__027_ -> (
      match interp expr__027_ with
      | ValIntT expr__027_' -> ValBool (M.is_empty expr__027_')
      | _ -> failwith "impossible: unary constructor")
    | Mem (int__028_, expr__029_) -> (
      match interp expr__029_ with
      | ValIntT expr__029_' -> ValBool (M.mem int__028_ expr__029_')
      | _ -> failwith "impossible: n-ary constructor")
    | Add (int__030_, expr__031_) -> (
      match interp expr__031_ with
      | ValIntT expr__031_' -> ValIntT (M.add int__030_ expr__031_')
      | _ -> failwith "impossible: n-ary constructor")
    | Rem (int__032_, expr__033_) -> (
      match interp expr__033_ with
      | ValIntT expr__033_' -> ValIntT (M.rem int__032_ expr__033_')
      | _ -> failwith "impossible: n-ary constructor")
    | Size expr__034_ -> (
      match interp expr__034_ with
      | ValIntT expr__034_' -> ValInt (M.size expr__034_')
      | _ -> failwith "impossible: unary constructor")
    | Union (expr__035_, expr__036_) -> (
      match (interp expr__035_, interp expr__036_) with
      | ValIntT expr__035_', ValIntT expr__036_' ->
        ValIntT (M.union expr__035_' expr__036_')
      | _ -> failwith "impossible: n-ary constructor")
    | Intersect (expr__037_, expr__038_) -> (
      match (interp expr__037_, interp expr__038_) with
      | ValIntT expr__037_', ValIntT expr__038_' ->
        ValIntT (M.intersect expr__037_' expr__038_')
      | _ -> failwith "impossible: n-ary constructor")
    | Invariant expr__039_ -> (
      match interp expr__039_ with
      | ValIntT expr__039_' -> ValBool (M.invariant expr__039_')
      | _ -> failwith "impossible: unary constructor")
end
