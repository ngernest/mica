open Ppxlib

(******************************************************************************)
(** {1 Longident utility functions} *)

(** Takes a [Location.t] value and marks it as a ghost location
    - This is useful for new AST nodes (to indicate that the node
      did not exist in the source file) *)
val ghostify : Location.t -> Location.t

(** Alias for [String.uncapitalize_ascii] *)
val uncapitalize : string -> string

(** Converts a [string] to a [Longident] 
    - Alias for [Longident.parse] *)
val lident_of_string : string -> Longident.t

(** Converts a string [x] at location [loc] to a [Longident] *)
val lident_loc_of_string : string -> loc:Location.t -> Longident.t Location.loc

(** Converts a [Longident] to a regular string *)
val uncapitalize_lident : Longident.t -> Longident.t

(** Only uncapitalizes the final [Lident] in a [Longident.t] 
    (prefixes in [Ldot]s are left unchanged) *)
val string_of_lident : Longident.t -> string

(** [add_lident_prefix p l] adds the prefix [p] to the identifier [l] 
    using dot notation, returning a new identifier [p.l] *)
val add_lident_prefix : string -> Longident.t -> Longident.t

(** [add_lident_loc_prefix] is like [add_lident_prefix], 
    but attaches location information to the resultant identifier *)
val add_lident_loc_prefix :
  string -> Longident.t Location.loc -> Longident.t Location.loc
