(** Generated property-based testing code *)
open Base
open Base_quickcheck

open StackInterface
open ListStack
open VariantStack

(** Suppress "unused value" compiler warnings *)
[@@@ocaml.warning "-27-32-33-34"]

type expr =
  | Empty
  | Push of int * expr
  | Pop of expr
  | Peek of expr
  | Clear of expr
  | Is_empty of expr
  | Length of expr
    [@@deriving sexp_of]

type ty =
  Bool | Int | IntOption | T | TOption | Unit
    [@@deriving sexp_of]

module ExprToImpl (M : StackInterface) = struct 
  include M

  type value = 
    | ValBool of bool
    | ValInt of int
    | ValIntOption of int option
    | ValT of int M.t
    | ValTOption of int M.t option
    | ValUnit of unit
      [@@deriving sexp_of]

  let rec interp (expr : expr) : value = 
    match expr with
     | Empty -> ValT (M.empty)
     | Push(x1, e2) ->
      begin match interp e2 with 
       | ValT e' -> ValT (M.push x1 e')
       | _ -> failwith "impossible"
      end
     | Pop e ->
      begin match interp e with 
       | ValT e' -> ValTOption (M.pop e')
       | _ -> failwith "impossible"
      end
     | Peek e ->
      begin match interp e with 
       | ValT e' -> ValIntOption (M.peek e')
       | _ -> failwith "impossible"
      end
     | Clear e ->
      begin match interp e with 
       | ValT e' -> ValUnit (M.clear e')
       | _ -> failwith "impossible"
      end
     | Is_empty e ->
      begin match interp e with 
       | ValT e' -> ValBool (M.is_empty e')
       | _ -> failwith "impossible"
      end
     | Length e ->
      begin match interp e with 
       | ValT e' -> ValInt (M.length e')
       | _ -> failwith "impossible"
      end

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
        G.return @@ Is_empty e
      in is_empty
   | (Int, _) ->
      let length = 
        let%bind e = G.with_size ~size:(k / 2) (gen_expr T) in 
        G.return @@ Length e
      in length
   | (IntOption, _) ->
      let peek = 
        let%bind e = G.with_size ~size:(k / 2) (gen_expr T) in 
        G.return @@ Peek e
      in peek
   | (T, _) ->
      let push = 
        let%bind x1 = G.int_inclusive (-10) 10 in
        let%bind e2 = G.with_size ~size:(k / 2) (gen_expr T) in 
        G.return @@ Push(x1, e2)
      in push
   | (TOption, _) ->
      let pop = 
        let%bind e = G.with_size ~size:(k / 2) (gen_expr T) in 
        G.return @@ Pop e
      in pop
   | (Unit, _) ->
      let clear = 
        let%bind e = G.with_size ~size:(k / 2) (gen_expr T) in 
        G.return @@ Clear e
      in clear

module I1 = ExprToImpl(ListStack)
module I2 = ExprToImpl(VariantStack)

let displayError (e : expr) (v1 : I1.value) (v2 : I2.value) : string = 
  Printf.sprintf "e = %s, v1 = %s, v2 = %s\n"
    (Sexp.to_string @@ sexp_of_expr e)
    (Sexp.to_string @@ [%sexp_of: I1.value] v1)
    (Sexp.to_string @@ [%sexp_of: I2.value] v2)