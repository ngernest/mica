open Core
open Base_quickcheck 
open Sets


(** Revised definition of expressions *)
type expr = 
  (* Base cases *)
  | Empty
  | EInt of int
  (* Commands that change the underlying state *)
  | Add of int * expr
  | Remove of int * expr
  | Union of expr * expr 
  | Intersection of expr * expr 
  (* Observable commands *)
  | Mem of int * expr 
  | Size of expr
  | Is_empty of expr  
  [@@deriving sexp_of, quickcheck]

(** ADT representing types, where [T] is the abstract type of the module *)
type ty = T | Int | Bool
  [@@deriving sexp_of, quickcheck]


(** Functor relating [expr] to the actual module [M] implementing [SetIntf] *)
module ExprToImpl (M : SetIntf) = struct 
  
  (** Type representing values, i.e. observations made *)
  type value = ValT of int M.t | ValInt of int | ValBool of bool


  (** [interp expr] interprets the expression [expr] wrt the module 
      implementation [M], and returns the result as a [value] *)     
  let rec interp (expr : expr) : value = 
    match expr with 
    | EInt n -> ValInt n
    | Empty -> ValT M.empty
    (* TODO: figure out how to combine the nested pattern-matches??? *)
    | Add (x, e) -> 
      (match interp e with 
      | ValT v -> ValT (M.add x v)
      | _ -> failwith "ill-typed")
    | Remove (x, e) -> 
      (match interp e with 
      | ValT v -> ValT (M.rem x v)
      | _ -> failwith "ill-typed")
    | Union (e1, e2) -> 
      (match interp e1, interp e2 with 
      | ValT v1, ValT v2 -> ValT (M.union v1 v2)
      | _ -> failwith "ill-typed")
    | Intersection (e1, e2) ->
      (match interp e1, interp e2 with 
      | ValT v1, ValT v2 -> ValT (M.inter v1 v2)
      | _ -> failwith "ill-typed")
    | Mem (x, e) -> 
      (match interp e with 
      | ValT v -> ValBool (M.mem x v)
      | _ -> failwith "ill-typed")
    | Size e -> 
      (match interp e with 
      | ValT v -> ValInt (M.size v)
      | _ -> failwith "ill-typed")
    | Is_empty e ->
      (match interp e with 
      | ValT v -> ValBool (M.is_empty v)
      | _ -> failwith "ill-typed")

end


(** Generator for expressions *)  
let rec gen_expr (ty : ty) : expr Generator.t = 
  let module G = Generator in 
  let open Generator.Let_syntax in 
  match ty with 
  | Int -> let%map n = G.int_uniform in EInt n
  | Bool -> 
    let memGen = 
      let%bind x = G.int_uniform in
      let%map e = gen_expr T in 
        Mem (x, e) in
    let isEmptyGen = 
      let%map e = gen_expr T in 
        Is_empty e in
      G.union [memGen; isEmptyGen]
  | T -> failwith "TODO"
  

