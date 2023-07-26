open Core
open Base_quickcheck

(** Utility functions for printing out distributions of reified values
    over the modules under test. 
    - Note: This module is intended to be [open]ed before the file containing 
      the auto-generated PBT code is [open]ed. Note that the type [expr] 
      is declared abstractly in the file, whereas [expr] is instantiated
      with a concrete definition in the auto-generated files produced
      by Mica (e.g. [lib/generated.ml]). [Open]ing this file 
      prior to [open]ing [lib/generated.ml] will allow these utility functions
      to operate on the concrete definition of the [expr] datatype. 
    - See [bin/GeneratedSetExecutable.ml] for an example of how to 
    invoke the functions in this file
*)

(** Abstract type of {i symbolic expressions} *)
type expr [@@deriving sexp, compare]

(** Module containing a comparator witness & serialization functions
    for the [expr] datatype. 
    This module is necessary for creating maps/sets over [expr]s -- 
    see {{: https://dev.realworldocaml.org/maps-and-hashtables.html} Chapter 14} 
    of {i Real World OCaml} for further details on this programming idiom. 
    (Also need to update the appropriate PPX macro invocations in 
    [GeneratedSetPBTCode.ml])    
*)
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