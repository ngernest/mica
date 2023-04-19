open Core
open Core.Quickcheck 
open Sets

(** ADT representing types, T1 & T2 are the abstract types of the two Set implementations *)
(** TODO: not sure how to associate T1 & T2 with the actual implementations 
    -- do we need a function mapping each Ti to the appropriate module satisfying SetIntf? *)
type ty = T1 | T2 | Int of int 

(** Type representing values, i.e. observations made *)
type value = IntV of int | BoolV of bool 

(** Commands that change the underlying state *)
type expr = 
  | Add of int * expr
  | Remove of int * expr
  | Union of expr * expr 
  | Intersection of expr * expr 
  | Empty
  [@@deriving sexp_of, quickcheck]
  
(** Observations that don't change the underlying state *)
type observable = 
  | Mem of int * expr 
  | Size of expr
  | Is_empty of expr  
  [@@deriving sexp_of, quickcheck]

(** Generator for expressions?  *)  
let gen_exp (t : ty) : expr Generator.t = failwith "TODO"

(** Interprets the symbolic command over the model / SUT *)
let interp_expr (expr : expr) : expr = failwith "TODO"  

(** Interprets observations over the model / SUT *)
let interp_obs (obs: observable) : value = failwith "TODO"
