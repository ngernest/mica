val expr_of_type : Ppxlib.core_type -> Ppxlib.expression

val generate_impl :
  ctxt:Ppxlib.Expansion_context.Deriver.t ->
  'a * Ppxlib.type_declaration list -> Ppxlib.structure_item list

val impl_generator :
  (Ppxlib.structure_item list,
   Ppxlib.rec_flag * Ppxlib.type_declaration list)
  Ppxlib.Deriving.Generator.t

  val stringify : Ppxlib.Deriving.t
