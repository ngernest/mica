(** Generated executable for testing observational equivalence of two modules *)
(** Usage: [dune exec -- ./bin/GeneratedMapExecutable.exe] *)
open Core
open Lib.GeneratedMapPBTCode

let () = 
  let module QC = Quickcheck in 
  QC.test (gen_expr AssocList) ~sexp_of:sexp_of_expr ~f:(fun e ->
    match (I1.interp e, I2.interp e) with 
     | (ValAssocList ps1, ValAssocList ps2) -> [%test_eq: Lib.AssocList.t] ps1 ps2
     | v1, v2 -> failwith @@ displayError e v1 v2);

  QC.test (gen_expr StringOption) ~sexp_of:sexp_of_expr ~f:(fun e ->
    match (I1.interp e, I2.interp e) with 
     | (ValStringOption s1, ValStringOption s2) -> [%test_eq: string option] s1 s2
     | v1, v2 -> failwith @@ displayError e v1 v2);