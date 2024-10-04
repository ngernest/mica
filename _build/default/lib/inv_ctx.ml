open Ppxlib
open StdLabels

(** "Inverse" typing context: maps types to variable names, implemented
   as an association list *)
type inv_ctx = (core_type * string) list

(** The empty "inverse" typing context *)
let empty_ctx : inv_ctx = []

(** [find_exprs gamma] extracts all the variables with type [expr] from the 
    inverse typing context [gamma] *)
let find_exprs (gamma : inv_ctx) : string list =
  List.fold_left
    ~f:(fun acc (ty, var) ->
      match ty with
      | [%type: expr] -> var :: acc
      | _ -> acc)
    ~init:[] gamma
