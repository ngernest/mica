open Base
open Base_quickcheck
open RegexMatcher
open Brzozowski 
open DFA 

(** Suppress unused value compiler warnings *)
[@@@ocaml.warning "-27-32-34"]

type expr = 
  | Void 
  | Empty 
  | Lit of char 
  | Alt of expr * expr 
  | Cat of expr * expr 
  | Star of expr 
  | MatchString of expr * string 
  | AcceptsEmpty of expr 
  [@@deriving sexp]

type ty = Bool | T
  [@@deriving sexp]

module ExprToImpl (M : RegexMatcher) = struct 
  include M
  
  type value = 
    | ValBool of bool 
    | ValT of M.t 
    [@@deriving sexp]

  let rec interp (expr : expr) : value = 
    match expr with 
    | Void -> ValT (M.void)
    | Empty -> ValT (M.empty)
    | Lit c -> ValT (M.lit c)
    | Alt(e1, e2) -> 
      begin match interp e1, interp e2 with 
      | ValT e1', ValT e2' -> ValT (M.alt e1' e2')
      | _, _ -> failwith "impossible"
      end 
    | Cat(e1, e2) -> 
      begin match interp e1, interp e2 with 
      | ValT e1', ValT e2' -> ValT (M.cat e1' e2')
      | _, _ -> failwith "impossible"
      end 
    | Star e -> 
      begin match interp e with 
      | ValT e' -> ValT (M.star e')
      | _ -> failwith "impossible"
      end 
    | MatchString (e, s) -> 
      begin match interp e with 
      | ValT e' -> ValBool (M.matchString e' s)
      | _ -> failwith "impossible"
      end
    | AcceptsEmpty e -> 
      begin match interp e with 
      | ValT e' -> ValBool (M.acceptsEmpty e')
      | _ -> failwith "impossible"
      end
end   

let rec gen_expr (ty : ty) : expr Generator.t = 
  let module G = Generator in 
  let open G.Let_syntax in 
  let%bind k = G.size in 
  match ty, k with 
  | (T, 0) -> G.union [return Void; return Empty]
  | (Bool, _) -> 
    let matchString = 
      let%bind s = G.string in 
      let%bind e = G.with_size ~size:(k / 2) (gen_expr T) in 
      G.return @@ MatchString(e, s) in 
    let acceptsEmpty = 
      let%bind e = G.with_size ~size:(k / 2) (gen_expr T) in 
      G.return @@ AcceptsEmpty e in 
    G.union [matchString; acceptsEmpty]
  | (T, _) -> 
    let lit = 
      let%bind c = G.char_alpha in 
      G.return @@ Lit c in 
    let alt = 
      let%bind e1 = G.with_size ~size:(k / 2) (gen_expr T) in 
      let%bind e2 = G.with_size ~size:(k / 2) (gen_expr T) in 
      G.return @@ Alt (e1, e2) in 
    let cat = 
      let%bind e1 = G.with_size ~size:(k / 2) (gen_expr T) in 
      let%bind e2 = G.with_size ~size:(k / 2) (gen_expr T) in 
      G.return @@ Cat (e1, e2) in  
    let star = 
      let%bind e = G.with_size ~size:(k / 2) (gen_expr T) in  
      G.return @@ Star e in 
    G.union [lit; alt; cat; star]

module I1 = ExprToImpl(Brzozowski)
module I2 = ExprToImpl(DFA)

let displayError (e : expr) (v1 : I1.value) (v2 : I2.value) : string = 
  Printf.sprintf "e = %s, v1 = %s, v2 = %s\n"
    (Sexp.to_string @@ sexp_of_expr e)
    (Sexp.to_string @@ [%sexp_of: I1.value] v1)
    (Sexp.to_string @@ [%sexp_of: I2.value] v2)



 



