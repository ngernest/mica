open Ppxlib

(** {1 Equality of [Parsetree] types)} *)

(** Checks if a [constructor_declaration] for the [ty] ADT and 
    (its corresponding) [core_type] are equal with respect to their string 
    representations using [string_of_core_ty].
    - e.g. this function returns [true] when [core_ty = bool]
    and [constructor_declaration = Bool]. *)
val equal_ty_cstr_core_type : constructor_declaration -> core_type -> bool

(** Checks two [Longident.t] values for equality *)
val equal_longident : Longident.t -> Longident.t -> bool

(** Checks two [Longident.t Location.loc] values for equality, 
    ignoring their location *)
val equal_longident_loc :
  Longident.t Location.loc -> Longident.t Location.loc -> bool

(** Checks two [core_type]s for equality, ignoring location *)
val equal_core_type : core_type -> core_type -> bool

(** Checks two [core_type_desc]s for equality, ignoring location.
    - Does not support objects, classes, polymorphic variants, 
      universally quantified types, packages or extension nodes *)
val equal_core_type_desc : core_type_desc -> core_type_desc -> bool

(** Checks two [core_type list]s for equality, ignoring location *)
val equal_core_type_list : core_type list -> core_type list -> bool

(** Checks two [mutable_flag]s for equality *)
val equal_mutable_flag : mutable_flag -> mutable_flag -> bool

(** Checks two [label_declaration]s for equality, ignoring location *)
val equal_label_declaration : label_declaration -> label_declaration -> bool

(** Checks two [constructor_argument]s for equality, ignoring location info *)
val equal_constructor_arguments :
  constructor_arguments -> constructor_arguments -> bool

(** Checks two [constructor_declaration]s for equality, ignoring location *)
val equal_constructor_declaration :
  constructor_declaration -> constructor_declaration -> bool
