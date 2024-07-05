open Ppxlib

(** {1 Miscellany} *)

(** Alias for [Stdio.printf] *)
val printf : ('a, Stdio.Out_channel.t, unit) format -> 'a

(** Constructs a [loc] given some payload [txt] and a location [loc] *)
val with_loc : 'a -> loc:Location.t -> 'a Location.loc

(** Strips the location info from a value of type ['a loc] *)
val no_loc : 'a Location.loc -> 'a

(** Maps a function [f] over a value of type ['a loc], 
    returning a value of type ['b loc] *)
val max_loc : Location.t -> Location.t -> Location.t

(** Takes the maximum of two [Location.t] values using Ppxlib's in-built 
    [Location.compare] function *)
val map_with_loc : f:('a -> 'b) -> 'a Location.loc -> 'b Location.loc

(** Maps a function component-wise over a pair *)
val map2 : f:('a -> 'b) -> 'a * 'a -> 'b * 'b

(** Converts a triple to a pair *)
val tuple4_to_pair : 'a * 'b * 'c * 'd -> 'a * 'b

(** Checks if a list is empty
    - Backwards-compatible version of [List.is_empty], 
    which is only available in OCaml 5.1 and newer *)
val list_is_empty : 'a list -> bool

(** Takes the disjunction of a Boolean list
    - The empty list corresponds to false
    - Reimplementation of the [or] function in 
      Haskell's [GHC.Prelude] *)
val list_or : bool list -> bool

(** Retrieves all elements of a list except the last one *)
val remove_last : 'a list -> 'a list

(** Returns the final element of a list (if one exists) 
    - Raises an exception if the list is empty *)
val get_last : 'a list -> 'a

(** [merge_list_with_assoc_list xs yzs ~eq] takes [xs : 'a list] 
    and an association list [yzs : ('b * 'c) list], and creates a 
    new association list of type [('a * 'c) list], using the function [eq] 
    to equate values of type ['a] and ['b] together
    - Raises an exception if there does not exist any element in [xs]
      that [eq] deems to be equal to a key in [yzs] *)
val merge_list_with_assoc_list :
  'a list -> ('b * 'c) list -> eq:('a -> 'b -> bool) -> ('a * 'c) list

(** Swaps the keys & values of an association list.
    - Note: bijectivity is not guaranteed since keys may appear more than once
    in the input association list.
    - Adapted from Jane street's [Base.List.Assoc.inverse] function *)
val invert_assoc_list : ('a * 'b) list -> ('b * 'a) list

(** Returns true the abstract type declaration in a [signature] 
    is parameterized (e.g. ['a t]), else returns [false] *)
val is_abs_ty_parameterized : signature -> bool
