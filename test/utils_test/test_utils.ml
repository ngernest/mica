open Lib__Utils

open Alcotest
open Ppxlib

(*******************************************************************************)
(* Boilerplate for making [core_type] a [testable] type in 
   the Alcotest harness *)

let pp_core_type = Ppxlib.Pprintast.core_type

let core_type_eq (t1 : core_type) (t2 : core_type) = 
  String.equal (Ppxlib.string_of_core_type t1) (Ppxlib.string_of_core_type t2)

let core_type_testable = Alcotest.testable pp_core_type core_type_eq

(*******************************************************************************)
(* Monomorphization preserves core types *)

(* Strip [core_types] of location info and expose the [loc] parameter
   (needed for Ppxlib quotations) *)
let loc = Location.none

let mono_int () = 
  Alcotest.check core_type_testable "mono_int" 
    (monomorphize [%type: int]) [%type: int]

let mono_string () = 
  Alcotest.check core_type_testable "mono_bool" 
    (monomorphize [%type: string]) [%type: string] 

let mono_bool () = 
  Alcotest.check core_type_testable "mono_bool" 
    (monomorphize [%type: bool]) [%type: bool] 

(*******************************************************************************)
(* Monomorphization instantiates type variables with [int] *)
let mono_list () = 
  Alcotest.check core_type_testable "mono_list"
    (monomorphize [%type: 'a list]) [%type: int list]

let mono_option () = 
  Alcotest.check core_type_testable "mono_option"
    (monomorphize [%type: 'a option]) [%type: int option]    

let mono_double_list () = 
  Alcotest.check core_type_testable "mono_double_list"
    (monomorphize [%type: ('a list) list]) [%type: (int list) list]

let mono_pair () = 
  Alcotest.check core_type_testable "mono_pair"
    (monomorphize [%type: 'a * 'b]) [%type: int * int]   

let mono_pair_list () = 
  Alcotest.check core_type_testable "mono_pair_list"
    (monomorphize [%type: ('a * 'b) list]) [%type: (int * int) list]

(*******************************************************************************)
(* Overall Alcotest Test Suite *)

let () = 
  run "Utils test suite" [
    ("[monomorphize] preserves base types", [
      test_case "int" `Quick mono_int;
      test_case "bool" `Quick mono_bool;
      test_case "string" `Quick mono_string;  
    ]);
    ("[monomorphize] instantiates type variables with [int]", [
      test_case "'a list" `Quick mono_list;
      test_case "'a option" `Quick mono_option;
      test_case "'a list list" `Quick mono_double_list; 
      test_case "'a * 'b" `Quick mono_pair;
      test_case "('a * 'b) list" `Quick mono_pair_list
    ])
  ]