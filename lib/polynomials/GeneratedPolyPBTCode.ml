open Base
(** Auto-generated property-based testing code *)

open Base_quickcheck
open PolyInterface
open Poly1
open Poly2

(** Suppress "unused value" compiler warnings *)
[@@@ocaml.warning "-27-32-33-34"]

type expr =
  | Zero
  | One
  | Power of int * int
  | Monomial of int * int
  | Add of expr * expr
  | Mult of expr * expr
  | Create of (int * int) list
  | Eval of expr * int
  | Equal of expr * expr
[@@deriving sexp_of]

type ty = Bool | Int | T [@@deriving sexp_of]

module ExprToImpl (M : PolyInterface) = struct
  include M

  type value = ValBool of bool | ValInt of int | ValT of M.t
  [@@deriving sexp_of]

  let rec interp (expr : expr) : value =
    match expr with
    | Zero -> ValT M.zero
    | One -> ValT M.one
    | Power (n1, n2) -> ValInt (M.power n1 n2)
    | Monomial (n1, n2) -> ValT (M.monomial n1 n2)
    | Add (e1, e2) -> (
        match (interp e1, interp e2) with
        | ValT e1', ValT e2' -> ValT (M.add e1' e2')
        | _ -> failwith "impossible")
    | Mult (e1, e2) -> (
        match (interp e1, interp e2) with
        | ValT e1', ValT e2' -> ValT (M.mult e1' e2')
        | _ -> failwith "impossible")
    | Create ps -> ValT (M.create ps)
    | Eval (e1, n2) -> (
        match interp e1 with
        | ValT e' -> ValInt (M.eval e' n2)
        | _ -> failwith "impossible")
    | Equal (e1, e2) -> (
        match (interp e1, interp e2) with
        | ValT e1', ValT e2' -> ValBool (M.equal e1' e2')
        | _ -> failwith "impossible")
end

let rec gen_expr (ty : ty) : expr Generator.t =
  let module G = Generator in
  let open G.Let_syntax in
  let%bind k = G.size in
  match (ty, k) with
  | T, 0 -> G.union [ G.return One; G.return Zero ]
  | Bool, _ ->
      let equal =
        let%bind e1 = G.with_size ~size:(k / 2) (gen_expr T) in
        let%bind e2 = G.with_size ~size:(k / 2) (gen_expr T) in
        G.return @@ Equal (e1, e2)
      in
      equal
  | Int, _ ->
      let power =
        let%bind n1 = G.small_positive_or_zero_int in
        let%bind n2 = G.small_positive_or_zero_int in
        G.return @@ Power (n1, n2)
      in
      let eval =
        let%bind e1 = G.with_size ~size:(k / 2) (gen_expr T) in
        let%bind n2 = G.small_positive_or_zero_int in
        G.return @@ Eval (e1, n2)
      in
      G.union [ power; eval ]
  | T, _ ->
      let monomial =
        let%bind n1 = G.small_positive_or_zero_int in
        let%bind n2 = G.small_positive_or_zero_int in
        G.return @@ Monomial (n1, n2)
      in
      let add =
        let%bind e1 = G.with_size ~size:(k / 2) (gen_expr T) in
        let%bind e2 = G.with_size ~size:(k / 2) (gen_expr T) in
        G.return @@ Add (e1, e2)
      in
      let mult =
        let%bind e1 = G.with_size ~size:(k / 2) (gen_expr T) in
        let%bind e2 = G.with_size ~size:(k / 2) (gen_expr T) in
        G.return @@ Mult (e1, e2)
      in
      let create =
        let%bind ps =
          G.list
          @@ G.both G.small_positive_or_zero_int G.small_positive_or_zero_int
        in
        G.return @@ Create ps
      in
      G.union [ monomial; add; mult; create ]

module I1 = ExprToImpl (Poly1)
module I2 = ExprToImpl (Poly2)

let displayError (e : expr) (v1 : I1.value) (v2 : I2.value) : string =
  Printf.sprintf "e = %s, v1 = %s, v2 = %s\n"
    (Sexp.to_string @@ sexp_of_expr e)
    (Sexp.to_string @@ [%sexp_of: I1.value] v1)
    (Sexp.to_string @@ [%sexp_of: I2.value] v2)
