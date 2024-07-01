open Ppxlib

(******************************************************************************)
(** {1 Longident utility functions} *)

val uncapitalize : string -> string
val lident_loc_of_string : string -> loc:Location.t -> Longident.t Location.loc
val uncapitalize_lident : Longident.t -> Longident.t
val string_of_lident : Longident.t -> string
val add_lident_prefix : string -> Longident.t -> Longident.t

val add_lident_loc_prefix :
  string -> Longident.t Location.loc -> Longident.t Location.loc

val map_with_loc : f:('a -> 'b) -> 'a Location.loc -> 'b Location.loc 
