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

(** Extracts the argument types of functions defined in the module signature,
    and generates ADT constructors that take these types as type parameters *)
let extractArgTypes (v : valDecl) : document = 
  let open String in 
  match valType v with 
  | Func1 (arg, _) -> 
    !^ (capitalize @@ valName v)
    ^^ (!^ " of ")
    ^^ (!^ (string_of_ty arg))
  | Func2 (arg1, arg2, _) -> 
    !^ (capitalize @@ valName v)
    ^^ !^ " of "
    ^^ !^ (string_of_ty ~alpha:"int" arg1)
    ^^ !^ " * "
    ^^ !^ (string_of_ty ~alpha:"int" arg2)
  | _ -> !^ (capitalize @@ valName v)

(** Generates the definition of the [expr] ADT *)  
let exprADTDecl (m : moduleSig) : document = 
  prefix 2 1 
  (!^ "type expr =")
  (group @@ separate_map (hardline ^^ !^ " | ") 
    extractArgTypes m.valDecls)  

(** Extracts the return type of a function as a document
    For non-arrow types, this function just extracts the type as a document *)    
let extractReturnTypes (v : valDecl) : string = 
  match valType v with 
  | Func1 (_, ret) | Func2 (_, _, ret) -> (string_of_ty ~t:"T" ret)
  | ty -> (string_of_ty ~t:"T" ty)

let tyADTDecl (m : moduleSig) : document = 
  let retTypes = List.dedup_and_sort ~compare:String.compare
    @@ List.map ~f:(fun ty -> extractReturnTypes ty |> String.capitalize) m.valDecls in 
  prefix 2 1
  (!^ "type ty =")
  (group @@ separate_map (!^ " | ") (!^) retTypes)