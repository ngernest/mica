(** This is the set interface that students were asked to implement using ordered
   lists & BSTs respectively. 
   - The actual implementations have been omitted from this repo (to avoid 
   releasing homework solutions online).*)

(******************************************************************************)
(* ABSTRACT SET INTERFACE                                                     *)
(******************************************************************************)

(* This module signature defines the values associated with the abstract type
   `'a set`. Any module struct that implements the `SET` interface must contain
   all of the values defined in this interface; otherwise it won't compile. *)

module type SET = sig
  (* The "abstract type" 'a set.

     A set is an _unordered_ collection of _distinct_ elements.

     By unordered, we mean that {4, 5, 6} and {5, 6, 4} are equivalent
     mathematically. By distinct, we mean that a set contains any particular
     element only either 0 or 1 times (no duplicates are allowed). *)
  type 'a set

  (* The empty set, with no elements. *)
  val empty : 'a set

  (* `is_empty s` is true exactly when s has no elements. *)
  val is_empty : 'a set -> bool

  (* `list_of_set s` returns a list of all the elements of s. This list should
     be sorted in ascending order and may not contain duplicate elements. *)
  val list_of_set : 'a set -> 'a list

  (* `add x s` returns a set just like s, except x is now also an element. If x
     is already an element of s, then it just returns s. *)
  val add : 'a -> 'a set -> 'a set

  (* `remove x s` returns a set just like s, except x is not an element. If x
     already wasn't an element, then it just returns s. *)
  val remove : 'a -> 'a set -> 'a set

  (* `member x s` returns true exactly when x is a member of s. *)
  val member : 'a -> 'a set -> bool

  (* `size s` returns the "cardinality" (number of elements) of s. *)
  val size : 'a set -> int

  (* `equals s1 s2` returns true exactly when s1 and s2 have the same elements.
     For example: equals {"a", "b"} {"b", "a"} = true equals {true} {false} =
     false *)
  val equals : 'a set -> 'a set -> bool

  (* `set_of_list l` returns a set containing all the elements of the list l. *)
  val set_of_list : 'a list -> 'a set
end
