(** Generated executable for testing observational equivalence of two modules *)
(** Usage: [dune exec -- ./bin/GeneratedStackExecutable.exe] *)
open Core
open Lib.GeneratedStackPBTCode

let () = 
  let module QC = Quickcheck in 
  QC.test (gen_expr Bool) ~sexp_of:sexp_of_expr ~f:(fun e ->
    match (I1.interp e, I2.interp e) with 
     | (ValBool b1, ValBool b2) -> [%test_eq: bool] b1 b2
     | v1, v2 -> failwith @@ displayError e v1 v2);

  QC.test (gen_expr Int) ~sexp_of:sexp_of_expr ~f:(fun e ->
    match (I1.interp e, I2.interp e) with 
     | (ValInt n1, ValInt n2) -> [%test_eq: int] n1 n2
     | v1, v2 -> failwith @@ displayError e v1 v2);

  QC.test (gen_expr IntOption) ~sexp_of:sexp_of_expr ~f:(fun e ->
    match (I1.interp e, I2.interp e) with 
     | (ValIntOption n1, ValIntOption n2) -> [%test_eq: int option] n1 n2
     | v1, v2 -> failwith @@ displayError e v1 v2);

  QC.test (gen_expr Unit) ~sexp_of:sexp_of_expr ~f:(fun e ->
    match (I1.interp e, I2.interp e) with 
     | (ValUnit u1, ValUnit u2) -> [%test_eq: unit] u1 u2
     | v1, v2 -> failwith @@ displayError e v1 v2);
