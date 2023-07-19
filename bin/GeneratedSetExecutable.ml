(** Generated executable for testing observational equivalence of two modules *)
(** Usage: [dune exec -- ./bin/GeneratedSetExecutable.exe] *)
open Core
open Lib.GeneratedSetPBTCode
open Base_quickcheck

(** Suppress "unused value" compiler warnings *)
[@@@ocaml.warning "-32"]

(** Module containing a comparator witness & serialization functions
    for the [expr] datatype. 
    This module is necessary for creating maps/sets over [expr]s -- 
    see {{: https://dev.realworldocaml.org/maps-and-hashtables.html} Chapter 14} 
    of {i Real World OCaml} for further details on this programming idiom. *)
module Expr = struct 
  module T = struct 
    type t = expr 
      [@@deriving sexp, compare]
  end   

  include T 
  include Comparator.Make(T)  
end

(** Given a sequence [seq] of generated [expr]s, [seq_coverage seq] 
    produces a [Map] which maps each unique [expr] to its frequency in [seq] *)
let seq_coverage (seq : expr Sequence.t) : int Map.M(Expr).t = 
  Generator.Debug.coverage (module Expr) seq

(** [genSeqPrintFreq g] takes a generator [g] of [expr]s, 
    uses [g] to produce a random sequence of [expr]s, and prints the frequency 
    of each [expr] in the random sequence to [stdout] *)  
let genSeqPrintFreq (g : expr Generator.t) : unit =   
  Printf.printf "Printing sequence\n";
  Test.with_sample_exn g ~f:(fun seq -> 
    let counts = seq_coverage seq in 
    counts |> [%sexp_of: int Map.M(Expr).t] |> print_s)

let m = Map.of_alist_exn (module Expr) [(Empty, 1)]

let incr_map m key = Map.update m key ~f:(fun n -> 
    let v = Option.value n ~default:0 in 
    v + 1)    


(** TODO: figure out how to hook up the utility functions above 
          with the QC code below *)    
let () = 
  let module QC = Core.Quickcheck in 
  QC.test (gen_expr Bool) ~sexp_of:sexp_of_expr ~f:(fun e ->
    match (I1.interp e, I2.interp e) with 
     | (ValBool b1, ValBool b2) -> [%test_eq: bool] b1 b2
     | v1, v2 -> failwith @@ displayError e v1 v2);

  QC.test (gen_expr Int) ~sexp_of:sexp_of_expr ~f:(fun e ->
    match (I1.interp e, I2.interp e) with 
     | (ValInt n1, ValInt n2) -> [%test_eq: int] n1 n2
     | v1, v2 -> failwith @@ displayError e v1 v2);
