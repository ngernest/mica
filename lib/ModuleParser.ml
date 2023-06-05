open Parser 
open ParserTypes
open Angstrom
open Base
open Stdio

(** A parser for module signatures, based on the 
    {{: https://github.com/inhabitedtype/angstrom } Angstrom } 
    parser-combinator library. 
    
    This file depends on helper functions defined in the module [Lib.Parser],
    and datatype definitions defined in [Lib.ParserTypes]. *)

(** Alias for the [Angstrom] module *)
module A = Angstrom

(** Parsers for identifiers in OCaml 
    [ident ::= (A..Z | a..z | _) {A..Z | a..z | 0..9 | _ | '}] *)    
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

(** Parses the S-expression PPX annotation, consuming subsequent whitespace *)  
let sexpAnnotP : unit A.t = 
  constP "[@@deriving sexp]" ()

(** Parser for an abstract type declaration in a module, eg. [type 'a t] *)
let abstractTypeDeclP : abstractType A.t = 
  let typeTokenP = stringP "type" in
  let noParam = typeTokenP *> constP "t" T0 in 
  let withParam = typeTokenP *> wsP typeParamP *> constP "t" (T1 Alpha) in
  (noParam <|> withParam) <* sexpAnnotP

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
  <|> constP "string" String
  <|> constP "\'a t" AlphaT
  <|> constP "\'a" Alpha 
  <|> constP "t" T

(** General parser for parameterized types, eg. options/pairs/lists *)  
let paramTypeP : ty A.t = 
  fix @@ fun paramType -> 
    let pairP = (fun (t1, t2) -> Pair (t1, t2)) 
      <$> A.both baseTypeP (stringP "*" *> baseTypeP) in 
    let optionP = 
      (fun ty -> Option ty) <$> ((baseTypeP <|> parens paramType) <* stringP "option") in 
    let listP = 
      (fun ty -> List ty) <$> ((baseTypeP <|> parens paramType) <* stringP "list") in 
    pairP <|> optionP <|> listP
    
(** General parser for non-arrow types *)
let typeP : ty A.t = 
  paramTypeP <|> baseTypeP

(** Parser for arrow types *)  
let arrowTypeP : ty A.t = 
  let func1P = (fun arg ret -> Func1 (arg, ret)) 
    <$> typeP <* stringP "->" <*> typeP in 
  let func2P = (fun arg1 arg2 ret -> Func2 (arg1, arg2, ret)) 
    <$> typeP <* stringP "->" <*> typeP <* stringP "->" <*> typeP in 
  (func2P <|> func1P)
    
(** Parser for a value declaration inside a module, 
    eg. [val empty : 'a t] or [val func : 'a -> 'a t -> 'a t] *)   
let valDeclP : valDecl A.t = 
  (fun valName valType -> { valName; valType }) 
    <$> stringP "val" *> lowercaseIdentP <* stringP ":" <*> (arrowTypeP <|> typeP)  


(** Parser for a module signature *)
let moduleTypeP : moduleSig A.t = 
  (fun moduleName abstractType valDecls -> 
    { moduleName; moduleType = Intf; abstractType; valDecls; intFlag = AllInts }) 
  <$> stringP "module type" *> modNameP <* stringP "= sig" 
  <*> abstractTypeDeclP
  <*> A.many valDeclP
  <* stringP "end"

(** Reads a file as a single string *)
let string_of_file (filename : string) : string = 
  In_channel.with_file filename 
    ~f:(fun input -> In_channel.input_all input) 

(** Writes a string to a file *)    
let file_of_string (filename : string) (str : string) = 
  Out_channel.with_file ~append:false ~fail_if_exists:false 
    filename ~f:(fun output -> Out_channel.output_string output str)  
