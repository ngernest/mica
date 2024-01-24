open Base
(** Auto-generated property-based testing code *)

open Base_quickcheck
open SetInterface
open ListSet
open BSTSet

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
[@@deriving sexp_of, compare, equal]

type ty = Bool | Int | T [@@deriving sexp_of]

module ExprToImpl (M : SetInterface) = struct
  include M

  type value = ValBool of bool | ValInt of int | ValT of int M.t
  [@@deriving sexp_of]

  let rec interp (expr : expr) : value =
    match expr with
    | Empty -> ValT M.empty
    | Is_empty e -> (
        match interp e with
        | ValT e' -> ValBool (M.is_empty e')
        | _ -> failwith "impossible")
    | Mem (x1, e2) -> (
        match interp e2 with
        | ValT e' -> ValBool (M.mem x1 e')
        | _ -> failwith "impossible")
    | Add (x1, e2) -> (
        match interp e2 with
        | ValT e' -> ValT (M.add x1 e')
        | _ -> failwith "impossible")
    | Rem (x1, e2) -> (
        match interp e2 with
        | ValT e' -> ValT (M.rem x1 e')
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
end

(** Normalizes an [expr] *)
let normalize (e : expr) : expr = 
  begin match e with 
  | Union (Empty, e')
  | Union (e', Empty) -> e'  
  | Intersect (Empty, _) 
  | Intersect (_, Empty) -> Empty 
  | Rem (_, Empty) -> Empty
  | _ -> e 
  end

(** Checks if an [expr] is non-trivial *)  
let nontrivial (e : expr) : bool =   
  begin match e with 
  | Union (Empty, _) 
  | Union (_, Empty)
  | Intersect (Empty, _) 
  | Intersect (_, Empty)
  | Is_empty Empty 
  | Mem (_, Empty) -> false 
  | Intersect (e1, e2) | Union (e1, e2) -> not (equal_expr e1 e2)
  | Add (x1, Add (x2, Empty)) -> not (x1 = x2)
  | _ -> true
  end

(* Note that we now take in a context [ctx] of previously generated int's *)
let rec gen_expr (ctx : int list) (ty : ty) : expr Generator.t =
  let module G = Generator in
  let open G.Let_syntax in
  let%bind k = G.size in
  let genAtom = G.of_list [1; 2; 3; 4; 5] in 
  let genFromCache = 
    if List.is_empty ctx then genAtom
    else G.weighted_union [ (0.8, G.of_list ctx); (0.2, genAtom) ] in
  begin match (ty, k) with
  | T, 0 -> return Empty
  | Bool, _ ->
      let is_empty =
        let%bind e = G.with_size ~size:(k / 2) (gen_expr ctx T) in
        G.return @@ Is_empty (normalize e)
      in
      let mem =
        let%bind x1 = genFromCache in 
        let%bind e2 = G.with_size ~size:(k / 2) (gen_expr (x1::ctx) T) in
        G.return @@ Mem (x1, normalize e2)
      in
      G.union [ is_empty; mem ]
  | Int, _ ->
      let size =
        let%bind e = G.with_size ~size:(k / 2) (gen_expr ctx T) in
        G.return @@ Size (normalize e)
      in
      size
  | T, _ ->
      let add =
        let%bind x1 = genFromCache in 
        let%bind e2 = G.with_size ~size:(k / 2) (gen_expr ctx T) in
        G.return @@ Add (x1, normalize e2)
      in
      let rem =
        let%bind x1 = genFromCache in
        let%bind e2 = G.with_size ~size:(k / 2) (gen_expr (x1::ctx) T) in
        G.return @@ Rem (x1, normalize e2)
      in
      let union =
        let%bind e1 = G.with_size ~size:(k / 2) (gen_expr ctx T) in
        let%bind e2 = G.with_size ~size:(k / 2) (gen_expr ctx T) in
        G.return @@ normalize (Union (normalize e1, normalize e2))
      in
      let intersect =
        let%bind e1 = G.with_size ~size:(k / 2) (gen_expr ctx T) in
        let%bind e2 = G.with_size ~size:(k / 2) (gen_expr ctx T) in
        G.return @@ normalize (Intersect (normalize e1, normalize e2))
      in
      G.union [ add; rem; union; intersect ]
  end

module I1 = ExprToImpl (ListSet)
module I2 = ExprToImpl (BSTSet)

let displayError (e : expr) (v1 : I1.value) (v2 : I2.value) : string =
  Printf.sprintf "e = %s, v1 = %s, v2 = %s\n"
    (Sexp.to_string @@ sexp_of_expr e)
    (Sexp.to_string @@ [%sexp_of: I1.value] v1)
    (Sexp.to_string @@ [%sexp_of: I2.value] v2)
