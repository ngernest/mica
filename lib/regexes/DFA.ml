open! Base 
open Regex

(** Representation of DFAs in OCaml
    - Adapted from {{: https://www.youtube.com/watch?v=QaMU0wMMczU} Harry Goldstein's video} on DFAs & regexes 
*)

(************************************************************************)
(** {1 String utility functions} *)
(* Taken from https://stackoverflow.com/questions/9863036/ocaml-function-parameter-pattern-matching-for-strings#9863069 *)

(** Converts a char to a string *)
let string_of_char (c : char) : string = String.make 1 c

(** [explode str] converts the string [str] to a list of chars *)
let explode (str : string) : char list =
  let rec explode_inner cur_index chars = 
    if cur_index < String.length str then
      let new_char = str.[cur_index] in
      explode_inner (cur_index + 1) (chars @ [new_char])
    else chars in
  explode_inner 0 []

(** [implode chars] converts [chars], a list of chars, to a string *)
let rec implode (chars : char list) =
  match chars with
  | [] -> ""
  | h::t ->  string_of_char h ^ (implode t)
(************************************************************************)
(** {1 DFA representation} *)

(** A coinductive representation of DFA states 
    - The [accepting] field tells us whether the current state is an accepting state 
    - The [next] record selector of type [dfa -> char -> dfa] 
      gives us the next state given the current character in the string *)
type dfa = 
  { accepting : bool; 
    next : char -> dfa 
  }
  [@@deriving fields]

(** A DFA for the regex [a(b* + c)] *)
let rec ex1 : dfa = 
  let open Char in 
  let rec fail : dfa lazy_t = 
    lazy { accepting = false ; next = Fn.const (force fail) } in
  let rec z = 
    { accepting = true; 
      next = fun c -> if c = 'b' then z else force fail } in
  let y = 
    { accepting = true; 
      next = Fn.const (force fail) } in
  let x = 
    { accepting = false;  
      next = fun c -> if c = 'c' then y 
                      else if c = 'b' then z 
                      else force fail } in 
  let start = 
    { accepting = false; 
      next = fun c -> if c = 'a' then x else force fail } in
  start

(** [runDFA s dfa] runs the DFA [dfa] over the string [s], 
    checking to see if [dfa] matches [s] *)  
let rec runDFA (s: string) (dfa : dfa) : bool = 
  match explode s with 
  | [] -> accepting dfa 
  | (c :: cs) -> runDFA (implode cs) (next dfa c)


(* TODO: finish adapting other DFA functions *)