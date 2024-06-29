open Ppxlib

(** {1 Pretty-printers} *)

val base_types : loc:Location.t -> core_type list
val pp_pattern : pattern -> unit
val pp_core_type : core_type -> unit
val pp_expression : expression -> unit
val pp_structure_item : structure_item -> unit
val monomorphize : core_type -> core_type
val string_of_core_ty : core_type -> string
