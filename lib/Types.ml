open Core
open Base_quickcheck 
open Sets


(** Revised definition of expressions *)
type expr = 
  (* Commands that change the underlying state *)
  | Add of int * expr
  | Remove of int * expr
  | Union of expr * expr 
  | Intersection of expr * expr 
  | Empty
  (* Observable commands *)
  | Mem of int * expr 
  | Size of expr
  | Is_empty of expr  
  [@@deriving sexp_of, quickcheck]

(** ADT representing types, where [T] is the abstract type of the module *)
type ty = T | Int | Bool


(** Functor relating [expr] to the actual module [M] implementing [SetIntf] *)
module ExprToImpl (M : SetIntf) = struct 
  
  (** Type representing values, i.e. observations made *)
  type value = ValT of int M.t | ValInt of int | ValBool of bool


  (** TODO: find a way to extract the abstract type [T] from the [expr] 
       similar to "bind" in Haskell ?? *)
  let extract (e : expr) : int M.t = failwith "TODO"

  (** [interp expr] interprets the expression [expr] wrt the module 
      implementation [M], and returns the result as a [value] *)
  let interp (expr : expr) : value =
    match expr with 
    | Add (x, s) -> ValT (M.add x (extract s))
    | Remove (x, s) -> ValT (M.rem x (extract s))
    | Union (s1, s2) -> ValT (M.union (extract s1) (extract s2))
    | Intersection (s1, s2) -> ValT (M.inter (extract s1) (extract s2))
    | Empty -> ValT M.empty
    | Mem (x, s) -> ValBool (M.mem x (extract s))
    | Size s -> ValInt (M.size (extract s))
    | Is_empty e -> ValBool (M.is_empty (extract s))
end



(** Generator for expressions *)  
let gen_expr (ty : ty) : expr Generator.t = 
  let module M = ListSetNoDups in 
  let s = Empty in 
  let module G = Generator in 
  match ty with 
  (** TODO: how to generate [T]'s ?? *)
  | T -> G.(union @@ 
    [ small_positive_or_zero_int >>= (fun x -> return @@ Add (x, s));
      small_positive_or_zero_int >>= (fun x -> return @@ Remove (x, s));
      return Empty
    ])
  | Int -> G.return @@ Size s
  | Bool -> G.(union @@ 
    [ small_positive_or_zero_int >>= (fun x -> return @@ Mem (x, s));
      return @@ Is_empty s
    ])
    



