open Parser 
open ParserTypes
open Angstrom
open Base

module A = Angstrom

(** Parsers for identifiers in OCaml 
    [ident ::= (A..Z | a..z | _) {A..Z | a..z | 0..9 | _ | '}]
*)    
let identP ?(firstCharP = letterP <|> underscoreP) () : string A.t = 
  let remainingChars = letterP <|> digitP <|> underscoreP <|> quoteP in 
  String.of_char_list <$> 
    wsP @@ A.lift2 (fun x acc -> x::acc) firstCharP (A.many remainingChars)   

(** Parser for identifiers which start with lowercase letters / an underscore *)    
let lowercaseIdentP : string A.t = 
  identP ~firstCharP:(lowercaseP <|> underscoreP) ()

(** Parses type-parameters, eg. ['a] *)    
let typeParamP : char A.t = 
  quoteP *> lowercaseP    
  
(** Parser for an abstract type declaration in a module, eg. [type 'a t] *)
let abstractTypeDeclP : abstractType A.t = 
  let typeP = stringP "type" in
  let noParam = typeP *> constP "t" T0 in 
  let withParam = typeP *> wsP typeParamP *> constP "t" (T1 Alpha) in
  noParam <|> withParam

(** [sigP p] takes a parser [p], and sandwiches it between parsers
    that parse the "sig" and "end" tokens *)
let sigP (p : 'a A.t) : 'a A.t = 
  between (stringP "sig") p (stringP "end")

(** Parser for module names, which must start with a capital letter *)  
let modNameP : string A.t = 
  identP ~firstCharP:(upperCaseP) ()  


(** Parser for base types *)    
let baseTypeP : ty A.t = 
  constP "int" Int 
  <|> constP "char" Char 
  <|> constP "bool" Bool 
  <|> constP "unit" Unit 
  <|> constP "\'a t" AlphaT 
  <|> constP "\'a" Alpha 
  <|> constP "t" T

(** Parser for arrow types *)  
let arrowTypeP : ty A.t = 
  let func1P = (fun arg ret -> Func1 (arg, ret)) 
    <$> baseTypeP <* stringP "->" <*> baseTypeP in 
  let func2P = (fun arg1 arg2 ret -> Func2 (arg1, arg2, ret)) 
    <$> baseTypeP <* stringP "->" <*> baseTypeP <* stringP "->" <*> baseTypeP in 
  (func2P <|> func1P)
    
(** Parser for a value declaration inside a module, 
    eg. [val empty : 'a t] or [val func : 'a -> 'a t -> 'a t] *)   
let valDeclP : valDecl A.t = 
  (fun valName valType -> { valName; valType }) 
    <$> stringP "val" *> lowercaseIdentP <* stringP ":" <*> (arrowTypeP <|> baseTypeP)  

(** Parser for a module signature *)
(* let moduleTypeP : t_module A.t = 
  (fun moduleName abstractType -> 
    { moduleName; moduleType = Intf; abstractType; values = None }) 
  <$> stringP "module type" *> modNameP <* stringP "=" <*> sigP abstractTypeDeclP *)


let moduleTypeP' : t_module A.t = 
  (fun moduleName abstractType valDecls -> 
    { moduleName; moduleType = Intf; abstractType; valDecls }) 
  <$> stringP "module type" *> modNameP <* stringP "= sig" 
  <*> abstractTypeDeclP 
  <*> A.many valDeclP
  <* stringP "end"

