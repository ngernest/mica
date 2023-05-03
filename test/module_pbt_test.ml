(* TODO: figure out how to import SetTypes *)

open Lib.SetTypes
open Core

let () = Quickcheck.test (gen_expr Bool) 
  ~f:(fun e -> 
  match I1.interp e, I2.interp e with 
  | ValBool b1, ValBool b2 ->
      [%test_eq: bool] b1 b2
  | _, _ -> (failwith "ill-typed"))