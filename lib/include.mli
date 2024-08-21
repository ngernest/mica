open Ppxlib 

(** Produces an [include] [structure_item] at location [loc] 
    for some [module_expr] [ME] *)
val include_module_expr :
  loc:Location.t -> Ppxlib.module_expr -> Ppxlib.structure_item

(** Produces an [include] [structure_item] at location [loc] for 
    some [structure] *)  
val include_structure :
  loc:Location.t -> Ppxlib.structure -> Ppxlib.structure_item
