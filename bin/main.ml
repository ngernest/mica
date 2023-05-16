open! Core
open! Angstrom
open! Core_unix
open! PPrint
open! Stdio

open! Lib.Parser
open! Lib.ParserTypes
open! Lib.ModuleParser
open! Lib.CodeGenerator

(** Writes a PPrint document to an Out_channel (eg. [stdout]) *)
let write_doc (outc : Out_channel.t) (doc : document) : unit = 
  ToChannel.pretty 1.0 60 outc doc

let () = 
  let filepath = "./lib/StackInterface.ml" in
  let moduleString = string_of_file filepath in 
  let sigName = getModuleSigName filepath in 
  match (run_parser moduleTypeP moduleString) with 
  | Ok m -> 
    let outc = Out_channel.create ~append:false "./lib/Gen2.ml" in
    (* Pretty-print code for importing libraries to [outc] *)
    write_doc outc 
      (imports filepath ^/^ exprADTDecl m ^/^ tyADTDecl m);
    write_doc outc 
      (functorDef m ~sigName:sigName ~functorName:"ExprToImpl");
    write_doc outc (genExprDef m);
    Out_channel.flush stdout;
    Out_channel.close outc

  | Error err -> printf "error = %s\n" err

  

(* let () = 
  let moduleString = string_of_file "./lib/SetInterface.ml" in 
  match (run_parser moduleTypeP moduleString) with 
  | Ok parsedModule -> 
    let outc = Out_channel.create ~append:false "./lib/Generated.ml" in
    (* Pretty-print code for importing libraries to [outc] *)
    ToChannel.pretty 1.0 60 outc 
      (imports ^/^ exprADTDecl parsedModule ^/^ tyADTDecl parsedModule);
    Out_channel.flush stdout;
    Out_channel.close outc

  | Error err -> printf "error = %s\n" err 
   *)



(* Current working directory is "module_pbt" *)

(* let () = 
  printf "\n";
  let moduleString = string_of_file "./lib/SetInterface.ml" in 
  match (run_parser moduleTypeP moduleString) with 
  | Ok ok -> printf "result = %s\n" 
    (Sexp.to_string @@ [%sexp_of: moduleSig] ok) 
  | Error err -> printf "error = %s\n" err *)

(* let moduleString = "module type M = sig                   \
                      type 'a t                           \
                      val empty : 'a t                    \
                      val func1 : 'a -> 'a t -> 'a t       \
                      val func2 : 'a -> 'a t -> 'a t        \
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