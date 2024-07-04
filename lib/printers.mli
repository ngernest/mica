open Ppxlib

(** {1 Pretty-printers} *)

(** List of OCaml base types 
    - The named argument [loc] is necessary in order for 
    the [Ppxlib.Metaquot] quotations to expand to the appropriate 
    AST fragments representing the base types. *)
val base_types : loc:Location.t -> core_type list

(** Pretty-printer for [pattern]s *)
val pp_pattern : pattern -> unit

(** Pretty-printer for [core_type]s *)
val pp_core_type : core_type -> unit

(** Pretty-printer for [expression]s *)
val pp_expression : expression -> unit

(** Pretty-printer for [structure_item]s *)
val pp_structure_item : structure_item -> unit

(** Instantiates all type variables ['a] inside a type expression with [int] 
  by recursing over the structure of the type expression. 
  Base types are left unchanged. 
  Note: this function only recurses over type expressions when 
  they consist of:
  - Type constructor applications ([Ptyp_constr])
  - Tuples ([Ptyp_tuple])
  - Arrow/function types ([Ptyp_arrow]). *)
val monomorphize : core_type -> core_type


(** Converts a type expression [ty] to its capitalized, camel-case 
    string representation (for use as a constructor in an algebraic data type) 
    - The type expression is monomorphized prior to computing its string
    representation (i.e. ['a] is instantiated to [int]).
    - Note: polymoprhic variants, objects, extensions/attributes are 
    not supported by this function.  
    - Note: this function is slightly different from [Ppxlib.string_of_core_type]
    due to its capitalization, camel-case & monomorphization functionalities. *)
val string_of_core_ty : core_type -> string
