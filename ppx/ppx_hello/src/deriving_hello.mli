val module_name_of_type : Ppxlib.type_declaration -> string option Location.loc

val str_gen :
  loc:Warnings.loc ->
  path:string ->
  'a * Ppxlib.type_declaration list -> Ppxlib.structure_item list

val sig_gen :
  loc:Warnings.loc ->
  path:'a -> 'b * Ppxlib.type_declaration list -> Ppxlib.signature_item list

val name : string
