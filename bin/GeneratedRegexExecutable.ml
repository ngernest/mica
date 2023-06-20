open Core
open Lib.GeneratedRegexPBTCode

let () = 
  let module QC = Quickcheck in 
  QC.test (gen_expr Bool) ~sexp_of:sexp_of_expr ~f:(fun e ->
    match (I1.interp e, I2.interp e) with 
     | (ValBool b1, ValBool b2) -> [%test_eq: bool] b1 b2
     | v1, v2 -> failwith @@ displayError e v1 v2);