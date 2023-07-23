(** Generated executable for testing observational equivalence of two modules *)
(** Usage: [dune exec -- ./bin/GeneratedRegexExecutable.exe] *)
open Core
open Lib.GeneratedRegexPBTCode
open Base_quickcheck

(** Suppress "unused value" compiler warnings *)
[@@@ocaml.warning "-32"]


(** Module containing a comparator witness & serialization functions
    for the [expr] datatype. 
    This module is necessary for creating maps/sets over [expr]s -- 
    see {{: https://dev.realworldocaml.org/maps-and-hashtables.html} Chapter 14} 
    of {i Real World OCaml} for further details on this programming idiom. 
    NB: we only need to [derive] one of [sexp] or [hash]: 
    - [derive sexp] if using [Map] module (purely functional maps)
    - [derive hash] if using [Hashtbl]
    (Also need to update the appropriate PPX macro invocations in 
    [GeneratedSetPBTCode.ml])    
*)
module Expr = struct 
  module T = struct 
    type t = expr 
      [@@deriving sexp, compare, hash]
  end   

  include T 
  include Comparator.Make(T)  
end

(** Given a sequence [seq] of generated [expr]s, [seq_coverage seq] 
    produces a [Map] which maps each unique [expr] to its frequency in [seq] *)
let seq_coverage (seq : expr Sequence.t) : int Map.M(Expr).t = 
  Generator.Debug.coverage (module Expr) seq

(** [genSeqPrintFreq gen] takes a generator [gen] of [expr]s, 
    uses [gen] to produce a random sequence of [expr]s, and prints the 
    frequency of each [expr] in the random sequence to [stdout] *)  
let genSeqPrintFreq (gen : expr Generator.t) : unit =   
  Printf.printf "Printing sequence\n";
  Test.with_sample_exn gen ~f:(fun seq -> 
    let counts = seq_coverage seq in 
    counts |> [%sexp_of: int Map.M(Expr).t] |> print_s)

(** [incrFreq m key] increments the frequency of [key] in the map [m] *)
let incrFreq (m : ('a, int, 'comparator) Map.t) 
             (key : 'a) : ('a, int, 'comparator) Map.t = 
  Map.update m key ~f:(fun n -> 
  let v = Option.value n ~default:0 in 
  v + 1)    

(** Increments the value of a key in a Hashtbl mapping ['a] values to [int] *)  
let incrHashtbl (h : ('a, int) Hashtbl.t) (key : 'a) : unit = 
  Hashtbl.update h key ~f:(fun n -> 
    let n' = Option.value n ~default:0 in 
    n' + 1)

(** [genExprWithLog gen h] takes a generator [gen] of [expr]s, 
    and produces a new generator that generates the same values 
    but also increments a hashtable [h] of frequencies (mapping [expr] to [int])
    every time a value is generated *)
let genExprWithLog (gen : expr Generator.t) 
                   (h : (expr, int) Hashtbl.t) : expr Generator.t = 
  Generator.Debug.monitor gen ~f:(incrHashtbl h)

(** Computes the sum of all the values in the hashmap [h] *)  
let sumValues (h : ('a, int) Hashtbl.t) : int = 
  Hashtbl.fold h ~init:0 ~f:(fun ~key:_ ~data acc -> acc + data) 

(** Prints the percentage of each [key]'s occurrence in the hashtable [h], 
    using [printKey] as the serialization for [key]s of type ['a] *)
let printPercent (h : ('a, int) Hashtbl.t) (printKey : 'a -> string) 
                 (key : 'a) : unit = 
  let open Float in 
  let occurrences = of_int (Hashtbl.find_exn h key) in 
  let total = of_int (sumValues h) in 
  printf "%s : %.2f%%\n" (printKey key) (occurrences /. total *. 100.0)


let () = 
  let open Printf in 
  let open Hashtbl in 
  let bh1 = create (module Bool) in 
  let bh2 = create (module Bool) in 

  let module QC = Core.Quickcheck in 
  QC.test (gen_expr Bool) ~sexp_of:sexp_of_expr ~f:(fun e ->
    match (I1.interp e, I2.interp e) with 
     | (ValBool b1, ValBool b2) -> incr bh1 b1; 
                                   incr bh2 b2;
                                   [%test_eq: bool] b1 b2
     | v1, v2 -> failwith @@ displayError e v1 v2);

  printf "\nBool distribution:\n";
  printf "Module 1:\n";
  iter_keys bh1 ~f:(printPercent bh1 string_of_bool);
  printf "Module 2:\n";
  iter_keys bh2 ~f:(printPercent bh2 string_of_bool);