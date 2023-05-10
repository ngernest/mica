open! Base
open! PPrint
open! ParserTypes

(** Document representing modules that need to be imported *)
let imports : document = 
  (string "open! Base") 
  ^^ hardline 
  ^^ (string "open! Base_quickcheck")

(** Document for the [expr] ADT definition which is generated
    from a module signature  *)
(* let exprADTDecl : document = 
  let open OCaml in 
  prefix 2 1 
  (!^ "type expr =")
  (variant "expr" "Empty" 1 []) *)


(** TODO: figure out how to extract arguments to constructors *)  
let exprADTDecl (m : moduleSig) : document = 
  prefix 2 1 
  (!^ "type expr =")
  (group @@ separate_map (!^ " | ") 
    (fun r -> valName r |> String.capitalize |> string) m.valDecls)


