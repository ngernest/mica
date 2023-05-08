open Parser 
open Angstrom
open Base

module P = Parser
module A = Angstrom

(** Parses the tokens [module type], consuming subsequent whitespace *)
let modTypeP : unit A.t = 
  stringP "module type"

(** Parser for module names, which msut *)  
let modNameP : string A.t = 
  let remainingChars = letterP <|> digitP <|> A.char '_' <|> A.char '\'' in
  String.of_char_list <$> 
    wsP @@ A.lift2 (fun x acc -> x::acc) upperCaseP (A.many remainingChars)

(** [sigP p] takes a parser [p], and sandwiches it between parsers
    that parse the "sig" and "end" tokens *)
let sigP (p : 'a A.t) : 'a A.t = 
  between (stringP "sig") p (stringP "end")
  