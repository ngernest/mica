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
    let open Core.Quickcheck.Generator.Let_syntax in
    "TODO"

  let _ = gen_expr

  module TestHarness (M : SetInterface) = struct
    include M

    type value = ValBool of bool | ValInt of int | ValIntT of int t

    let rec interp e =
      match e with
      | Empty -> ValIntT M.empty
      | Is_empty e1 -> (
        match interp e1 with
        | ValIntT e1' -> ValBool (M.is_empty e1')
        | _ -> failwith "impossible: unary constructor")
      | Mem (x1, e2) -> (
        match interp e2 with
        | ValIntT e2' -> ValBool (M.mem x1 e2')
        | _ -> failwith "impossible: n-ary constructor")
      | Add (x1, e2) -> (
        match interp e2 with
        | ValIntT e2' -> ValIntT (M.add x1 e2')
        | _ -> failwith "impossible: n-ary constructor")
      | Rem (x1, e2) -> (
        match interp e2 with
        | ValIntT e2' -> ValIntT (M.rem x1 e2')
        | _ -> failwith "impossible: n-ary constructor")
      | Size e1 -> (
        match interp e1 with
        | ValIntT e1' -> ValInt (M.size e1')
        | _ -> failwith "impossible: unary constructor")
      | Union (e1, e2) -> (
        match (interp e1, interp e2) with
        | ValIntT e1', ValIntT e2' -> ValIntT (M.union e1' e2')
        | _ -> failwith "impossible: n-ary constructor")
      | Intersect (e1, e2) -> (
        match (interp e1, interp e2) with
        | ValIntT e1', ValIntT e2' -> ValIntT (M.intersect e1' e2')
        | _ -> failwith "impossible: n-ary constructor")
      | Invariant e1 -> (
        match interp e1 with
        | ValIntT e1' -> ValBool (M.invariant e1')
        | _ -> failwith "impossible: unary constructor")

    let _ = interp
  end
end [@@ocaml.doc "@inline"]

[@@@end]
