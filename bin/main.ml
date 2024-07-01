module type SetInterface = sig
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
[@@deriving_inline mica_types, mica]

include struct
  [@@@ocaml.warning "-60"]

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

  let gen_expr ty =
    let open Core.Quickcheck.Generator in
    let open Let_syntax in
    [%bind
      let x = small_non_negative_int in
      return x]

  let _ = gen_expr

  module TestHarness (M : SetInterface) = struct
    include M

    type value = ValBool of bool | ValInt of int | ValIntT of int t

    let rec interp e =
      match e with
      | Empty -> ValIntT M.empty
      | Is_empty expr__001_ -> (
        match interp expr__001_ with
        | ValIntT expr__001_' -> ValBool (M.is_empty expr__001_')
        | _ -> failwith "impossible: unary constructor")
      | Mem (int__002_, expr__003_) -> (
        match interp expr__003_ with
        | ValIntT expr__003_' -> ValBool (M.mem int__002_ expr__003_')
        | _ -> failwith "impossible: n-ary constructor")
      | Add (int__004_, expr__005_) -> (
        match interp expr__005_ with
        | ValIntT expr__005_' -> ValIntT (M.add int__004_ expr__005_')
        | _ -> failwith "impossible: n-ary constructor")
      | Rem (int__006_, expr__007_) -> (
        match interp expr__007_ with
        | ValIntT expr__007_' -> ValIntT (M.rem int__006_ expr__007_')
        | _ -> failwith "impossible: n-ary constructor")
      | Size expr__008_ -> (
        match interp expr__008_ with
        | ValIntT expr__008_' -> ValInt (M.size expr__008_')
        | _ -> failwith "impossible: unary constructor")
      | Union (expr__009_, expr__010_) -> (
        match (interp expr__009_, interp expr__010_) with
        | ValIntT expr__009_', ValIntT expr__010_' ->
          ValIntT (M.union expr__009_' expr__010_')
        | _ -> failwith "impossible: n-ary constructor")
      | Intersect (expr__011_, expr__012_) -> (
        match (interp expr__011_, interp expr__012_) with
        | ValIntT expr__011_', ValIntT expr__012_' ->
          ValIntT (M.intersect expr__011_' expr__012_')
        | _ -> failwith "impossible: n-ary constructor")
      | Invariant expr__013_ -> (
        match interp expr__013_ with
        | ValIntT expr__013_' -> ValBool (M.invariant expr__013_')
        | _ -> failwith "impossible: unary constructor")

    let _ = interp
  end
end [@@ocaml.doc "@inline"]

[@@@end]
