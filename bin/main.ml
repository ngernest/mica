module type SetInterface = sig
  (* TODO: support [[@@deriving sexp]] annotations after the [type 'a t] declaration? *)
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

(* Auto-generated code *)
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

  module ExprToImpl (M : SetInterface) = struct end
end [@@ocaml.doc "@inline"]

[@@@end]
