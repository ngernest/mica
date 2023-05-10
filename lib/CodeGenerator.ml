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

(** Converts functions to ADT constructors for the [expr] ADT *)
let extractFuncTypes (v : valDecl) = 
  let open String in 
  match valType v with 
  | Func1 (arg, _) -> 
    !^ (capitalize @@ valName v)
    ^^ (!^ " of ")
    ^^ (!^ (string_of_ty arg))
  | Func2 (arg1, arg2, _) -> 
    !^ (capitalize @@ valName v)
    ^^ !^ " of "
    ^^ !^ (string_of_ty arg1)
    ^^ !^ " * "
    ^^ !^ (string_of_ty arg2)
  | _ -> !^ (capitalize @@ valName v)


(** TODO: figure out how to extract arguments to constructors *)  
let exprADTDecl (m : moduleSig) : document = 
  prefix 2 1 
  (!^ "type expr =")
  (group @@ separate_map (hardline ^^ !^ " | ") 
    extractFuncTypes m.valDecls)  
