(** Generated property-based testing code *)
open Base
open Base_quickcheck
open MapInterface
open AssocListMap
open RedBlackMap
open Latin

(** Suppress unused value compiler warnings *)
[@@@ocaml.warning "-27-32-34"]
  
module G = Generator

type ty = AssocList | StringOption | T
    [@@deriving sexp]

type expr =
  | Empty
  | Insert of (int * string) * expr
  | Find of int * expr
  | Remove of int * expr
  | From_list of AssocList.assoc_list
  | Bindings of expr
    [@@deriving sexp]
 
module ExprToImpl (M : MapInterface) = struct 
  include M

  type value = 
    | ValAssocList of AssocList.assoc_list
    | ValStringOption of string option
    | ValT of M.t
    [@@deriving sexp]   
  
  let rec interp (expr : expr) : value = 
    match expr with
     | Empty -> ValT (M.empty)
     | Insert(p1, e2) ->
      begin match interp e2 with 
       | ValT e' -> ValT (M.insert p1 e')
       | _ -> failwith "impossible"
      end
     | Find(n1, e2) ->
      begin match interp e2 with 
       | ValT e' -> ValStringOption (M.find n1 e')
       | _ -> failwith "impossible"
      end
     | Remove(n1, e2) ->
      begin match interp e2 with 
       | ValT e' -> ValT (M.remove n1 e')
       | _ -> failwith "impossible"
      end
     | From_list ps -> ValT (M.from_list ps)
     | Bindings e ->
      begin match interp e with 
       | ValT e' -> ValAssocList (M.bindings e')
       | _ -> failwith "impossible"
      end
end        

let rec gen_expr (ty : ty) : expr Generator.t = 
  let module G = Generator in 
  let open G.Let_syntax in 
  let%bind k = G.size in 
  match ty, k with 
  | (T, 0) -> return Empty
  | (AssocList, _) ->
      let bindings = 
        let%bind e = G.with_size ~size:(k / 2) (gen_expr T) in 
        G.return @@ Bindings e
      in bindings
  | (StringOption, _) ->
      let find = 
        let%bind n1 = G.int_inclusive (-10) 10 in
        let%bind e2 = G.with_size ~size:(k / 2) (gen_expr T) in 
        G.return @@ Find(n1, e2)
      in find
  | (T, _) ->
      let insert = 
        let%bind (n1, s2) as p1 = G.both (G.int_inclusive (-10) 10) genLatin in
        let%bind e2 = G.with_size ~size:(k / 2) (gen_expr T) in 
        G.return @@ Insert(p1, e2) in 
      let remove = 
        let%bind n1 = G.int_inclusive (-10) 10 in
        let%bind e2 = G.with_size ~size:(k / 2) (gen_expr T) in 
        G.return @@ Remove(n1, e2) in 
      let from_list = 
        let%bind ps = AssocList.gen_assoc_list in 
        G.return @@ From_list ps
      in G.union [ insert; remove; from_list ]

module I1 = ExprToImpl(AssocListMap)
module I2 = ExprToImpl(RedBlackMap)

let displayError (e : expr) (v1 : I1.value) (v2 : I2.value) : string = 
  Printf.sprintf "e = %s, v1 = %s, v2 = %s\n"
    (Sexp.to_string @@ sexp_of_expr e)
    (Sexp.to_string @@ [%sexp_of: I1.value] v1)
    (Sexp.to_string @@ [%sexp_of: I2.value] v2)