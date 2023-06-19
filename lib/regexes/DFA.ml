open Base 
open Regex
open Base_quickcheck

(** Representation of DFAs in OCaml
    - Adapted from {{: https://www.youtube.com/watch?v=QaMU0wMMczU} Harry Goldstein's video} on DFAs & regexes 
*)

(************************************************************************)
(** {1 String utility functions} *)
(* Taken from https://stackoverflow.com/questions/9863036/ocaml-function-parameter-pattern-matching-for-strings#9863069 *)

(** Converts a char to a string *)
let string_of_char (c : char) : string = 
  String.make 1 c

(** [explode str] converts the string [str] to a list of chars *)
let explode (str : string) : char list =
  let rec explode_inner cur_index chars = 
    if cur_index < String.length str then
      let new_char = str.[cur_index] in
      explode_inner (cur_index + 1) (chars @ [new_char])
    else chars in
  explode_inner 0 []

(** [implode chars] converts [chars], a list of chars, to a string *)
let rec implode (chars : char list) : string =
  match chars with
  | [] -> ""
  | h::t ->  string_of_char h ^ (implode t)

(************************************************************************)
(** {1 DFA representation} *)

(** A coinductive representation of DFA states 
    - The [acceptsEmpty] field tells us whether the current state is an accepting state 
    - The [next] record selector of type [dfa -> char -> dfa] 
      gives us the next state given the current character in the string *)
type dfa = 
  { acceptsEmpty : bool; [@sexp.bool]
    next : char -> dfa   [@sexp.opaque]
  }
  [@@deriving fields, sexp]

(** A DFA for the regex [a(b* + c)] *)
let ex1 : dfa = 
  let open Char in 
  let rec fail : dfa lazy_t = 
    lazy { acceptsEmpty = false ; next = Fn.const (force fail) } in
  let rec z = 
    { acceptsEmpty = true; 
      next = fun c -> if c = 'b' then z else force fail } in
  let y = 
    { acceptsEmpty = true; 
      next = Fn.const (force fail) } in
  let x = 
    { acceptsEmpty = false;  
      next = fun c -> if c = 'c' then y 
                      else if c = 'b' then z 
                      else force fail } in 
  let start = 
    { acceptsEmpty = false; 
      next = fun c -> if c = 'a' then x else force fail } in
  start

(** [matchString s dfa] runs the DFA [dfa] over the string [s], 
    checking to see if [dfa] matches [s] *)  
let rec matchString (dfa : dfa) (s: string) : bool = 
  match explode s with 
  | [] -> acceptsEmpty dfa 
  | (c :: cs) -> matchString (next dfa c) (implode cs)

let rec void : dfa lazy_t = 
  lazy { acceptsEmpty = false; next = Fn.const (force void) }

let rec empty : dfa lazy_t = 
  lazy { acceptsEmpty = true; next = Fn.const (force empty) }

let rec alt (x : dfa) (y : dfa) : dfa = 
  { acceptsEmpty = acceptsEmpty x || acceptsEmpty y;
    next = fun c -> alt (next x c) (next y c) 
  } 

let rec star (dfa : dfa) : dfa = 
  { acceptsEmpty = true;
    next = fun c -> alt (next dfa c) (star dfa)
  } 

let lit (c : char) : dfa = 
  let open Char in 
  { acceptsEmpty = false; 
    next = fun c' -> if c = c' then star (force void) 
                     else (force void)
  }

let rec cat (x : dfa) (y : dfa) : dfa = 
  { acceptsEmpty = acceptsEmpty x && acceptsEmpty y;
    next = fun c -> alt (cat (next x c) y) 
                        (if acceptsEmpty x then next y c else (force void))
  }
  
(** [convert re] converts the regex [re] to a DFA *)  
let rec convert (re : re) : dfa = 
  match re with 
  | Void -> force void 
  | Empty -> force empty
  | Lit c -> lit c 
  | Alt (r1, r2) -> alt (convert r1) (convert r2)
  | Cat (r1, r2) -> cat (convert r1) (convert r2)
  | Star r -> star (convert r)

(************************************************************************)
(** {1 QuickCheck generators for DFAs} *)  

module G = Generator
open! G.Let_syntax

let genDFA : dfa G.t = failwith "TODO: implement"


(* let genDFAString (dfa : dfa) : (string G.t) option = 
  failwith "TODO: do I have to first convert the DFA to a regex first, and then get the corresponding string"  *)