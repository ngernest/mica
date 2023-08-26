(* * Generated property-based testing code
open Base
open Base_quickcheck
open SetInterface
open ListSet
open BSTSet

(* NB: the [EInt] variant is currently not produced by Mica *)
type _ expr =
  | Empty : 'a expr 
  | EInt : int -> int expr  
  | Is_empty : 'a expr -> bool expr 
  | Mem : 'a * 'a expr -> bool expr 
  | Add : 'a * 'a expr -> 'a expr 
  | Rem : 'a * 'a expr -> 'a expr
  | Size : 'a expr -> int expr 
  | Union : 'a expr * 'a expr -> 'a expr 
  | Intersect : 'a expr * 'a expr -> 'a expr 
  | Invariant : 'a expr -> bool expr 

type ty =
  Bool | Int | T
    [@@deriving sexp]

module ExprToImpl (M : SetInterface) = struct 
  include M
  type _ value = 
    | ValBool : bool -> bool value 
    | ValInt : int -> int value 
    | ValT : int M.t -> int M.t value 

  let interp_value (type a) (value : a value) : a = 
    match value with 
    | ValInt v -> v
    | ValBool v -> v
    | ValT v -> v

  (** TODO: revisit this, look at the GADT chapter of 
      Real World OCaml *)
  (* let rec interp (type a) (expr : a expr) : a = 
    match expr with 
    | Empty -> interp_value (ValT M.empty)
    | _ -> failwith "TODO" *)
    

  end

let rec gen_expr (ty : ty) : expr Generator.t = 
  let module G = Generator in 
  let open G.Let_syntax in 
  let%bind k = G.size in 
  match ty, k with 
   | (T, 0) -> return Empty
   | (Bool, _) ->
      let is_empty = 
        let%bind e = G.with_size ~size:(k / 2) (gen_expr T) in 
        G.return @@ Is_empty e in 
      let mem = 
        let%bind x1 = G.int_inclusive (-10) 10 in 
        let%bind e2 = G.with_size ~size:(k / 2) (gen_expr T) in 
        G.return @@ Mem(x1, e2) in 
      let invariant = 
        let%bind e = G.with_size ~size:(k / 2) (gen_expr T) in 
        G.return @@ Invariant e
      in G.union [ is_empty; mem; invariant ]
   | (Int, _) ->
      let size = 
        let%bind e = G.with_size ~size:(k / 2) (gen_expr T) in 
        G.return @@ Size e in
      let eint = 
        let%bind n = G.int_inclusive (-10) 10 in 
        G.return @@ EInt n in
      G.union [ size; eint ]
    | (T, _) ->
      let add = 
        let%bind x1 = G.int_inclusive (-10) 10 in 
        let%bind e2 = G.with_size ~size:(k / 2) (gen_expr T) in 
        G.return @@ Add(x1, e2) in 
      let rem = 
        let%bind x1 = G.int_inclusive (-10) 10 in 
        let%bind e2 = G.with_size ~size:(k / 2) (gen_expr T) in 
        G.return @@ Rem(x1, e2) in 
      let union = 
        let%bind e1 = G.with_size ~size:(k / 2) (gen_expr T) in 
        let%bind e2 = G.with_size ~size:(k / 2) (gen_expr T) in 
        G.return @@ Union(e1, e2) in 
      let intersect = 
        let%bind e1 = G.with_size ~size:(k / 2) (gen_expr T) in 
        let%bind e2 = G.with_size ~size:(k / 2) (gen_expr T) in 
        G.return @@ Intersect(e1, e2)
      in G.union [ add; rem; union; intersect ]
   

module I1 = ExprToImpl(ListSet)
module I2 = ExprToImpl(BSTSet)

let displayError (e : expr) (v1 : I1.value) (v2 : I2.value) : string = 
  Printf.sprintf "Error: Generated [expr] was %s, Module 1 produced %s, 
  Module 2 produced %s"
    (Sexp.to_string @@ sexp_of_expr e)
    (Sexp.to_string @@ [%sexp_of: I1.value] v1)
    (Sexp.to_string @@ [%sexp_of: I2.value] v2) *)