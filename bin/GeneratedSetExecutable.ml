(** Generated executable for testing observational equivalence of two modules *)
(** Usage: [dune exec -- ./bin/GeneratedSetExecutable.exe] *)

(* Suppress "unused value" compiler warnings *)
[@@@ocaml.warning "-27-32-33-34"]

open Core
open Lib.Stats
open Lib.GeneratedSetPBTCode
open Lib.Utils

(* let () = 
  let open Or_error in 
  let module QC = Quickcheck in 
  let test_bool = QC.test_or_error (gen_expr Bool) ~sexp_of:sexp_of_expr 
    ~f:(fun e ->
      match (I1.interp e, I2.interp e) with 
      | ValBool b1, ValBool b2 -> 
          try_with ~backtrace:false (fun () -> [%test_eq: bool] b1 b2) 
      | v1, v2 -> error_string @@ displayError e v1 v2) in 

  let test_int = QC.test_or_error (gen_expr Int) ~sexp_of:sexp_of_expr 
    ~f:(fun e ->
      match (I1.interp e, I2.interp e) with 
      | ValInt n1, ValInt n2 -> 
          try_with ~backtrace:false (fun () -> [%test_eq: int] n1 n2) 
      | v1, v2 -> error_string @@ displayError e v1 v2) in
  
  match combine_errors_unit [test_bool; test_int] with 
  | Ok ok -> ok
  | Error err -> 
    let open Stdlib.Format in 
    Error.pp err_formatter err;
    print_newline () *)
   

(** Commented out QC code below that records stats using the 
    utility functions from [lib/Stats.ml] *)  
let () =   
  let open Printf in 
  let open Hashtbl in 
  let bh1 = create (module Bool) in 
  let bh2 = create (module Bool) in 

  let ih1 = create (module Int) in 
  let ih2 = create (module Int) in 

  let module QC = Core.Quickcheck in 
  QC.test (gen_expr Bool) ~sexp_of:sexp_of_expr ~f:(fun e ->
    match (I1.interp e, I2.interp e) with 
     | ValBool b1, ValBool b2 -> incr bh1 b1; 
                                 incr bh2 b2;
                                 [%test_eq: bool] b1 b2
     | v1, v2 -> failwith @@ displayError e v1 v2);

  QC.test (gen_expr Int) ~sexp_of:sexp_of_expr ~f:(fun e ->
    match (I1.interp e, I2.interp e) with 
     | (ValInt n1, ValInt n2) -> incr ih1 n1; 
                                 incr ih2 n2;
                                 [%test_eq: int] n1 n2    
     | v1, v2 -> failwith @@ displayError e v1 v2);

  let (bh1', bh2') = map2 ~f:sortByPercent (bh1, bh2) in 
  let (ih1', ih2') = map2 ~f:sortByPercent (ih1, ih2) in 
  
  printf "\nBool distribution:\n";
  printf "\t Module 1 \t Module 2 \n";
  List.iter2_exn bh1' bh2' ~f:(printPercents ~f:string_of_bool ~numChars:5);

  printf "\nInt distribution:\n";
  printf "\t Module 1 \t Module 2 \n";
  List.iter2_exn ih1' ih2' ~f:(printPercents ~f:string_of_int ~numChars:3);  