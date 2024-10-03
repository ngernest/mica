open Ppxlib
open StdLabels
open Miscellany

(** {1 Longident utility functions} *)

(** Takes a [Location.t] value and marks it as a ghost location
    - This is useful for new AST nodes (to indicate that the node
      did not exist in the source file) *)
let ghostify (loc : Location.t) : Location.t = { loc with loc_ghost = true }

(** Alias for [String.uncapitalize_ascii] *)
let uncapitalize : string -> string = String.uncapitalize_ascii

(** Converts a [string] to a [Longident] 
    - Alias for [Longident.parse] *)
let lident_of_string : string -> Longident.t = Longident.parse

(** Converts a string [x] at location [loc] to a [Longident] *)
let lident_loc_of_string (x : string) ~(loc : Location.t) :
  Longident.t Location.loc =
  with_loc (Longident.parse x) ~loc

(** Converts a [Longident] to a regular string *)
let string_of_lident (lident : Longident.t) : string =
  let xs = Astlib.Longident.flatten lident in
  match xs with
  | [] -> ""
  | [ x ] -> x
  | _ -> String.concat ~sep:"." xs

(** Maps a [string -> string] function via structural recursion over a [Longident.t] 
    (prefixes in [Ldot]s are left unchanged) *)
let rec map_lident ~(f : string -> string) (l : Longident.t) : Longident.t =
  match l with
  | Lident s -> Lident (f s)
  | Ldot (prefix, s) -> Ldot (prefix, f s)
  | Lapply (l1, l2) -> Lapply (map_lident ~f l1, map_lident ~f l2)

(** Uncapitalizes the final [Lident] in a [Longident.t] 
    (prefixes in [Ldot]s are left unchanged) *)
let uncapitalize_lident (l : Longident.t) : Longident.t =
  map_lident ~f:uncapitalize l

(** Capitalizes the final [Lident] in a [Longident.t] 
    (prefixes in [Ldot]s are left unchanged) *)
let capitalize_lident (l : Longident.t) : Longident.t =
  map_lident ~f:String.capitalize_ascii l

(** [add_lident_prefix p l] adds the prefix [p] to the identifier [l] 
    using dot notation, returning a new identifier [p.l] *)
let add_lident_prefix (prefix : string) (lident : Longident.t) : Longident.t =
  Ldot (Lident prefix, string_of_lident (uncapitalize_lident lident))

(** [add_lident_loc_prefix] is like [add_lident_prefix], 
    but attaches location information to the resultant identifier *)
let add_lident_loc_prefix (prefix : string)
  ({ txt; loc } : Longident.t Location.loc) : Longident.t Location.loc =
  with_loc ~loc @@ add_lident_prefix prefix txt
