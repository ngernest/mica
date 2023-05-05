open! Lib.SetTypes
open! Core


let () =
  let module QC = Quickcheck in 
  QC.test (gen_expr Bool) ~sexp_of:sexp_of_expr ~f:(fun e ->
      match (I1.interp e, I2.interp e) with
      | ValBool b1, ValBool b2 -> 
          [%test_eq: bool] b1 b2
      | v1, v2 -> failwith @@ 
          Printf.sprintf "e = %s, v1 = %s, v2 = %s\n" 
          (Sexp.to_string @@ sexp_of_expr e) 
          (Sexp.to_string @@ [%sexp_of: I1.value] v1) 
          (Sexp.to_string @@ [%sexp_of: I2.value] v2))      

(* Uncomment to test exprs that return Int *)          
(* let () =
  let module QC = Quickcheck in 
  QC.test (gen_expr Int) ~sexp_of:sexp_of_expr ~f:(fun e ->
      match (I1.interp e, I2.interp e) with
      | ValInt n1, ValInt n2 -> 
          [%test_eq: int] n1 n2
      | v1, v2 -> failwith @@ 
          Printf.sprintf "e = %s, v1 = %s, v2 = %s\n" 
          (Sexp.to_string @@ sexp_of_expr e) 
          (Sexp.to_string @@ [%sexp_of: I1.value] v1) 
          (Sexp.to_string @@ [%sexp_of: I2.value] v2))        *)