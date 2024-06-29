open Ppxlib

(** {1 Miscellany} *)

val printf : ('a, Stdio.Out_channel.t, unit) format -> 'a
val with_loc : 'a -> loc:Location.t -> 'a Location.loc
val no_loc : 'a Location.loc -> 'a
val map2 : f:('a -> 'b) -> 'a * 'a -> 'b * 'b
val tuple4_to_pair : 'a * 'b * 'c * 'd -> 'a * 'b
val list_is_empty : 'a list -> bool
val list_or : bool list -> bool
val remove_last : 'a list -> 'a list
val get_last : 'a list -> 'a

val merge_list_with_assoc_list :
  'a list -> ('b * 'c) list -> eq:('a -> 'b -> bool) -> ('a * 'c) list

val invert_assoc_list : ('a * 'b) list -> ('b * 'a) list
val abstract_ty_name : string
val add_prime : string -> string
