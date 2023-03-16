(* Implementation by Harry Goldstein, Dec 2022 *)
open! Core

(** Abstract interface for a priority queue *)
module type S = sig
  type value
  type priority
  type t

  val empty : t
  val insert : t -> value:value -> priority:priority -> t
  val extract : t -> (priority * value * t) option
end

(** [P] represents the priority 
    [V] represents the value to be inserted into the priority queue *)
module M (P : Comparable) (V : Comparable) :
  S with type priority = P.t and type value = V.t



  