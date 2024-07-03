open Ppxlib

val mk_cstr :
  name:string ->
  loc:Location.t ->
  arg_tys:core_type list ->
  constructor_declaration

val mk_adt :
  loc:Location.t ->
  name:string ->
  cstrs:constructor_declaration list ->
  type_declaration

val mk_error :
  local:Location.t ->
  global:Location.t ->
  (extension, Format.formatter, unit, extension) format4 ->
  structure_item

val attr : loc:Location.t -> name:string -> attribute

val mk_valt_pat :
  ?abs_ty_parameterized:bool -> string -> loc:Location.t -> pattern

val mk_scrutinees :
  string list ->
  post:(expression list -> expression) ->
  loc:Location.t ->
  expression
