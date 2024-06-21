(** Auto-generated property-based testing code *)
open Base

open Base_quickcheck

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

type ty = Bool | Int | T

module ExprToImpl (M : SetInterface) = struct
  include M

  type value = ValBool of bool | ValInt of int | ValT of int M.t

  let rec interp (expr : expr) : value =
    match expr with
    | Empty -> ValT M.empty
    | Is_empty e -> (
      match interp e with
      | ValT e' -> ValBool (M.is_empty e')
      | _ -> failwith "impossible")
    | Mem (e1, e2) -> (
      match interp e2 with
      | ValT e' -> ValBool (M.mem e1 e')
      | _ -> failwith "impossible")
    | Add (e1, e2) -> (
      match interp e2 with
      | ValT e' -> ValT (M.add e1 e')
      | _ -> failwith "impossible")
    | Rem (e1, e2) -> (
      match interp e2 with
      | ValT e' -> ValT (M.rem e1 e')
      | _ -> failwith "impossible")
    | Size e -> (
      match interp e with
      | ValT e' -> ValInt (M.size e')
      | _ -> failwith "impossible")
    | Union (e1, e2) -> (
      match (interp e1, interp e2) with
      | ValT e1', ValT e2' -> ValT (M.union e1' e2')
      | _ -> failwith "impossible")
    | Intersect (e1, e2) -> (
      match (interp e1, interp e2) with
      | ValT e1', ValT e2' -> ValT (M.intersect e1' e2')
      | _ -> failwith "impossible")
    | Invariant e -> (
      match interp e with
      | ValT e' -> ValBool (M.invariant e')
      | _ -> failwith "impossible")
end

let rec gen_expr (ty : ty) : expr Generator.t =
  let open Generator in
  let open Let_syntax in
  size >>= fun k ->
  match (ty, k) with
  | T, 0 -> return Empty
  | Bool, _ ->
    let is_empty =
      let g = with_size ~size:(k / 2) (gen_expr T) in
      g >>| fun e -> Is_empty e in
    let mem =
      let g1 = int_inclusive (-10) 10 in
      let g2 = with_size ~size:(k / 2) (gen_expr T) in
      both g1 g2 >>| fun (e1, e2) -> Mem (e1, e2) in
    let invariant =
      let g = with_size ~size:(k / 2) (gen_expr T) in
      g >>| fun e -> Invariant e in
    union [ is_empty; mem; invariant ]
  | Int, _ ->
    let size =
      let g = with_size ~size:(k / 2) (gen_expr T) in
      g >>| fun e -> Size e in
    size
  | T, _ ->
    let add =
      let g1 = int_inclusive (-10) 10 in
      let g2 = with_size ~size:(k / 2) (gen_expr T) in
      both g1 g2 >>| fun (e1, e2) -> Add (e1, e2) in
    let rem =
      let g1 = int_inclusive (-10) 10 in
      let g2 = with_size ~size:(k / 2) (gen_expr T) in
      both g1 g2 >>| fun (e1, e2) -> Rem (e1, e2) in
    let gen_union =
      let g1 = with_size ~size:(k / 2) (gen_expr T) in
      let g2 = with_size ~size:(k / 2) (gen_expr T) in
      both g1 g2 >>| fun (e1, e2) -> Union (e1, e2) in
    let intersect =
      let g1 = with_size ~size:(k / 2) (gen_expr T) in
      let g2 = with_size ~size:(k / 2) (gen_expr T) in
      both g1 g2 >>| fun (e1, e2) -> Intersect (e1, e2) in
    union [ add; rem; gen_union; intersect ]
