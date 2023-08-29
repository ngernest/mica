open Base
open Base_quickcheck
open PolyInterface
open Poly1
open Poly2

type expr =
  | Zero
  | One
  | Monomial of int * int
  | Add of expr * expr
  | Mult of expr * expr
  | Create of (int * int) list
  | Eval of expr * int
  | Equal of expr * expr
[@@deriving sexp]

type ty = Bool | Int | T [@@deriving sexp]

module ExprToImpl (M : PolyInterface) = struct
  include M

  type value = ValBool of bool | ValInt of int | ValT of M.t [@@deriving sexp]

  let rec interp (expr : expr) : value =
    match expr with
    | Zero -> ValT M.zero
    | One -> ValT M.one
    | Monomial (coeff, deg) -> ValT (M.monomial coeff deg)
    | Add (e1, e2) -> (
        match (interp e1, interp e2) with
        | ValT e1', ValT e2' -> ValT (M.add e1' e2')
        | _ -> failwith "impossible")
    | Mult (e1, e2) -> (
        match (interp e1, interp e2) with
        | ValT e1', ValT e2' -> ValT (M.mult e1' e2')
        | _ -> failwith "impossible")
    | Create lst -> ValT (M.create lst)
    | Eval (e, x) -> (
        match interp e with
        | ValT e' -> ValInt (M.eval e' x)
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
  | T, 0 -> G.union [ return Zero; return One ]
  | Bool, _ ->
      let equal =
        let%bind e1 = G.with_size ~size:(k / 2) (gen_expr T) in
        let%bind e2 = G.with_size ~size:(k / 2) (gen_expr T) in
        G.return @@ Equal (e1, e2)
      in
      equal
  | Int, _ ->
      let eval =
        let%bind e = G.with_size ~size:(k / 2) (gen_expr T) in
        let%bind x = G.int_inclusive (-10) 10 in
        G.return @@ Eval (e, x)
      in
      eval
  | T, _ ->
      let monomial =
        let%bind coeff = G.int_inclusive (-10) 10 in
        let%bind deg = G.small_strictly_positive_int in
        G.return @@ Monomial (coeff, deg)
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
        let%bind x1, x2 =
          G.both G.small_strictly_positive_int G.small_strictly_positive_int
        in
        G.return @@ Create [ (x1, x2) ]
      in
      (* let%bind lst = G.list genPairs in
         G.return @@ Create(lst) in *)
      G.union [ monomial; add; mult; create ]

module I1 = ExprToImpl (Poly1)
module I2 = ExprToImpl (Poly2)

let displayError (e : expr) (v1 : I1.value) (v2 : I2.value) : string =
  Printf.sprintf "e = %s, v1 = %s, v2 = %s\n"
    (Sexp.to_string @@ sexp_of_expr e)
    (Sexp.to_string @@ [%sexp_of: I1.value] v1)
    (Sexp.to_string @@ [%sexp_of: I2.value] v2)
