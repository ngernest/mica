open Ppxlib
open StdLabels
open Miscellany

(** {1 Longident utility functions} *)

(** Alias for [String.uncapitalize_ascii] *)
let uncapitalize = String.uncapitalize_ascii

let lident_loc_of_string (x : string) ~(loc : Location.t) :
  Longident.t Location.loc =
  with_loc (Longident.parse x) ~loc

(** Converts a [Longident] to a regular string, *)
let string_of_lident (lident : Longident.t) : string =
  let xs = Astlib.Longident.flatten lident in
  match xs with
  | [] -> ""
  | [ x ] -> x
  | _ -> String.concat ~sep:"." xs

(** Only uncapitalizes the final [Lident] in a [Longident.t] 
    (prefixes in [Ldot]'s are left unchanged) *)
let rec uncapitalize_lident (lident : Longident.t) : Longident.t =
  match lident with
  | Lident s -> Lident (uncapitalize s)
  | Ldot (prefix, s) -> Ldot (prefix, uncapitalize s)
  | Lapply (l1, l2) -> Lapply (uncapitalize_lident l1, uncapitalize_lident l2)

(** [add_lident_prefix p l] adds the prefix [p] to the identifier [l] 
    using dot notation, returning a new identifier [p.l] *)
let add_lident_prefix (prefix : string) (lident : Longident.t) : Longident.t =
  Ldot (Lident prefix, string_of_lident (uncapitalize_lident lident))

(** [add_lident_loc_prefix] is like [add_lident_prefix], 
    but attaches location information to the resultant identifier *)
let add_lident_loc_prefix (prefix : string)
  ({ txt; loc } : Longident.t Location.loc) : Longident.t Location.loc =
  with_loc ~loc @@ add_lident_prefix prefix txt
