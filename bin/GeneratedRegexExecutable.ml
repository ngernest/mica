(** Generated executable for testing observational equivalence of two modules *)
(** Usage: [dune exec -- ./bin/GeneratedRegexExecutable.exe] *)
open Core
open Lib.GeneratedRegexPBTCode


(** TODO: replace refs below with the more generic hashtable implementation 
    for keeping track of the frequencies of random [value]s 
    - See [GeneratedSetExecutable.ml] *)
let i1_true = ref 0 
let i1_false = ref 0 
let i2_true = ref 0
let i2_false = ref 0

let incr (ref : int ref) : unit = 
  ref := !ref + 1

let compare_bools (b1 : bool) (b2 : bool) : unit = 
  match b1, b2 with 
  | true, true -> incr i1_true; incr i2_true
  | true, false -> incr i1_true; incr i2_false
  | false, true -> incr i1_false; incr i2_true
  | false, false -> incr i1_false; incr i2_false

let () = 
  let module G = Base_quickcheck.Generator in 
  let open Printf in
  Quickcheck.test (gen_expr Bool) ~sexp_of:sexp_of_expr ~f:(fun e ->
    match (I1.interp e, I2.interp e) with 
     | (ValBool b1, ValBool b2) -> compare_bools b1 b2;
                                   [%test_eq: bool] b1 b2
     | v1, v2 -> failwith @@ displayError e v1 v2);

  printf "\nRESULTS\n";
  printf "I1 true = %d\n" !i1_true;
  printf "I1 false = %d\n" !i1_false;
  printf "I2 true = %d\n" !i2_true;
  printf "I2 false = %d\n" !i2_false;     
  printf "Total examples = %d\n" (!i1_true + !i1_false);
  