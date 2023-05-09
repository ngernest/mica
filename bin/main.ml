open! Core
open! Angstrom

open! Lib.Parser
open! Lib.ParserTypes
open! Lib.ModuleParser

let () = 
  printf "\n";
  match (run_parser valDeclP "val func : 'a -> 'a t -> 'a t") with 
  | Ok ok -> printf "result = %s\n" 
    (Sexp.to_string @@ [%sexp_of: valDecl] ok) 
  | Error err -> printf "error = %s\n" err

(* let moduleString = "module type M = sig \
                      type 'a t        \
                    end"

let () = 
  printf "\n";
  match (run_parser moduleTypeP moduleString) with 
  | Ok ok -> printf "result = %s\n" 
    (Sexp.to_string @@ [%sexp_of: t_module] ok) 
  | Error err -> printf "error = %s\n" err *)


(* Set example: [expr]s that return [Bool] *)          
(* let () =
  let open Lib.SetTypes in
  let module QC = Quickcheck in 
  QC.test (gen_expr Bool) ~sexp_of:sexp_of_expr ~f:(fun e ->
      match (I1.interp e, I2.interp e) with
      | ValBool b1, ValBool b2 -> 
          [%test_eq: bool] b1 b2
      | v1, v2 -> failwith @@ 
          Printf.sprintf "e = %s, v1 = %s, v2 = %s\n" 
          (Sexp.to_string @@ sexp_of_expr e) 
          (Sexp.to_string @@ [%sexp_of: I1.value] v1) 
          (Sexp.to_string @@ [%sexp_of: I2.value] v2))       *)



(****************************************************************)          
(* Set example: [expr]s that return [Int] *)          
(* let () =
  let open Lib.SetTypes in
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

(* Stack example: [expr]s that return [Char] *)
(* let () =
  let open Lib.StackTypes in 
  let module QC = Quickcheck in 
  QC.test (gen_expr Char) ~sexp_of:sexp_of_expr ~f:(fun e ->
      (* printf "e = %s\n" (Sexp.to_string @@ sexp_of_expr e); *)
      match (I1.interp e, I2.interp e) with
      | ValChar n1, ValChar n2 -> 
          (* printf "n1 = %c, n2 = %c\n" n1 n2; *)
          [%test_eq: char] n1 n2
      | v1, v2 -> failwith @@ 
          Printf.sprintf "e = %s, v1 = %s, v2 = %s\n" 
          (Sexp.to_string @@ sexp_of_expr e) 
          (Sexp.to_string @@ [%sexp_of: I1.value] v1) 
          (Sexp.to_string @@ [%sexp_of: I2.value] v2))    *)          