open Ppxlib

(** {1 Miscellany} *)

val printf : ('a, Stdio.Out_channel.t, unit) format -> 'a
val with_loc : 'a -> loc:Location.t -> 'a Location.loc
val no_loc : 'a Location.loc -> 'a
val max_loc : Location.t -> Location.t -> Location.t
val map_with_loc : f:('a -> 'b) -> 'a Location.loc -> 'b Location.loc
val liftF2 : f:('a -> 'b -> 'c) -> 'a Location.loc -> 'b Location.loc -> 'c Location.loc 


val map2 : f:('a -> 'b) -> 'a * 'a -> 'b * 'b
val tuple4_to_pair : 'a * 'b * 'c * 'd -> 'a * 'b
val list_is_empty : 'a list -> bool
val list_or : bool list -> bool
val remove_last : 'a list -> 'a list
val get_last : 'a list -> 'a

val merge_list_with_assoc_list :
  'a list -> ('b * 'c) list -> eq:('a -> 'b -> bool) -> ('a * 'c) list

val invert_assoc_list : ('a * 'b) list -> ('b * 'a) list
val is_abs_ty_parameterized : signature -> bool
