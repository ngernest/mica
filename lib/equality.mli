open Ppxlib

(** {1 Equality of [Parsetree] types)} *)

val equal_ty_cstr_core_type : constructor_declaration -> core_type -> bool
val equal_longident : Longident.t -> Longident.t -> bool
val equal_longident_loc : Longident.t Location.loc -> Longident.t Location.loc -> bool 
val equal_core_type : core_type -> core_type -> bool
val equal_core_type_desc : core_type_desc -> core_type_desc -> bool
val equal_core_type_list : core_type list -> core_type list -> bool
val equal_mutable_flag : mutable_flag -> mutable_flag -> bool
val equal_label_declaration : label_declaration -> label_declaration -> bool

val equal_constructor_arguments :
  constructor_arguments -> constructor_arguments -> bool

val equal_constructor_declaration :
  constructor_declaration -> constructor_declaration -> bool
