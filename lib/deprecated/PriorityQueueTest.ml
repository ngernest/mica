(* open! Core
open! Core.Poly
open! Core.Quickcheck
open! PriorityQueue

(* TODO: complete this module signature *)

module PQTest = struct

  (* TODO: figure out what's an appropriate model for a priority queue*)
  type state = Heap

  type sut = M(Int)(String).t

end


module PQT = TestHarness.Make(PQTest);; *)