(* open Lib.SetTypes
open Lib.Sets *)
(* open Core *)

(* 
module I1 = ExprToImpl(ListSetNoDups)
module I2 = ExprToImpl(ListSetDups) *)

(* let () = Quickcheck.test (gen_expr Bool) 
  ~f:(fun e -> 
  match I1.interp e, I2.interp e with 
  | ValBool b1, ValBool b2 ->
      [%test_eq: bool] b1 b2
  | _, _ -> (failwith "ill-typed")) *)


(* Uncomment to generate [expr]s that return [int] *)  
(* let () = Quickcheck.test (gen_expr Int) 
  ~f:(fun e -> 
  match I1.interp e, I2.interp e with 
  | ValInt n1, ValInt n2 ->
      [%test_eq: int] n1 n2
  | _, _ -> (failwith "ill-typed")) *)
