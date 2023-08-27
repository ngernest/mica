(** Auto-generated executable for testing observational equivalence of two modules *)
(** Usage: [dune exec -- ./bin/GeneratedRegexExecutable.exe] *)

(** Suppress "unused value" compiler warnings *)
[@@@ocaml.warning "-27-32-33-34"]

open Core
open Lib.Stats
open Lib.GeneratedRegexPBTCode
open Lib.Utils

let () = 
  let open Or_error in 
  let module QC = Quickcheck in 
  let test_bool = QC.test_or_error (gen_expr Bool) ~sexp_of:sexp_of_expr 
    ~f:(fun e ->
      match (I1.interp e, I2.interp e) with 
       | (ValBool b1, ValBool b2) ->
        try_with ~backtrace:false (fun () -> [%test_eq: bool] b1 b2)
       | v1, v2 -> error_string @@ displayError e v1 v2) in

  match combine_errors_unit [ test_bool ] with 
   | Ok ok -> ok
   | Error err ->
    let open Stdlib.Format in 
    Error.pp err_formatter err;
    print_newline ()

(* let () =   
  let open Printf in 
  let open Hashtbl in 

  (* let eh = create (module ObservableExpr) in  *)

  let bh1 = create (module Bool) in 
  let bh2 = create (module Bool) in 

  let ebh = create (module ExprBool) in 

  let module QC = Core.Quickcheck in 
  QC.test (gen_expr Bool) ~sexp_of:sexp_of_expr ~f:(fun e ->
    let e' = last e in 
    match (I1.interp e, I2.interp e) with 
    | ValBool b1, ValBool b2 -> 
      incr bh1 b1; incr bh2 b2; 
      (* incr eh e'; *)
      incr ebh (e', b1, b2);
      [%test_eq: bool] b1 b2;
    | v1, v2 -> failwith @@ displayError e v1 v2);
  
  let (bh1', bh2') = map2 ~f:sortByPercent (bh1, bh2) in 
  (* let eh' = sortByPercent eh in  *)
  let ebh' = sortByPercent ebh in 

  printf "\n Distribution of last-called Exprs and booleans: \n";
  List.iter ebh' 
    ~f:(printPercent 
      ~printKey:(fun (e, b1, b2) -> 
          let s = sexp_of_observableExpr e |> Sexp.to_string in 
          Printf.sprintf "%s %b %b" s b1 b2)
      ~precision:30);

  (* printf "\n Distribution of last-called Exprs: \n";
  List.iter eh' 
    ~f:(printPercent 
      ~:(fun e -> sexp_of_observableExpr e |> Sexp.to_string) 
      ~precision:15); *)
  
  printf "\nBool distribution:\n";
  printf "\t Module 1 \t Module 2 \n";
  List.iter2_exn bh1' bh2' 
    ~f:(printPercents ~printKey:string_of_bool ~precision:boolPrecision); *)
