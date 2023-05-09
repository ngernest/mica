open Parser 
open ParserTypes
open Angstrom
open Base

module P = Parser
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
let abstractTypeP : abstractType A.t = 
  let typeP = stringP "type" in
  let noParam = typeP *> constP "t" T in 
  let withParam = typeP *> wsP typeParamP *> constP "t" (T1 Alpha) in
  noParam <|> withParam

(** [sigP p] takes a parser [p], and sandwiches it between parsers
    that parse the "sig" and "end" tokens *)
let sigP (p : 'a A.t) : 'a A.t = 
  between (stringP "sig") p (stringP "end")

(** Parses the tokens [module type], consuming subsequent whitespace *)
let modTypeP : unit A.t = 
  stringP "module type"  

(** Parser for module names, which must start with a capital letter *)  
let modNameP : string A.t = 
  identP ~firstCharP:(upperCaseP) ()  

(** Parser for a module signature *)
let moduleTypeP : t_module A.t = 
  (fun moduleName abstractType -> { moduleName; moduleType = Intf; abstractType} ) 
    <$> stringP "module type" *> modNameP <* stringP "=" <*> sigP abstractTypeP
  
   