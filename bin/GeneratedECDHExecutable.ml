(** Generated executable for testing observational equivalence of two modules *)
(** Usage: [dune exec -- ./bin/GeneratedECDHExecutable.exe] *)
open Core
open Lib.GeneratedECDHPBTCode

let () = 
  let module QC = Quickcheck in 
  QC.test (gen_expr String) ~sexp_of:sexp_of_expr ~f:(fun e ->
    match (I1.interp e, I2.interp e) with 
     | (ValString s1, ValString s2) -> [%test_eq: string] s1 s2
     | v1, v2 -> failwith @@ displayError e v1 v2);