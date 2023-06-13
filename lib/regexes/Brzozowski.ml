open Base 

(** Type of regular expressions *)
type re = 
  | Void  (** always fails *)
  | Empty (** accepts empty string *) 
  | Char of char       (** Single literal character *)
  | Alt of re * re     (** [r1|r2], alternation *)
  | Cat of re * re     (** [r1 r2], concatenation *)
  | Star of re         (** [r*], Kleene star *)
  [@@deriving sexp, compare]

(** Smart constructor for [Alt], where [r1 <|> r2 = Alt (r1, r2)] *)
let (<|>) r1 r2 : re = 
  match r1, r2 with 
  | _, Void -> r1 
  | Void, _ -> r2
  | _, _ -> Alt (r1, r2)

(** Smart constructor for concatenation, where [r1 ^^ r2 = Cat (r1, r2)] *)
let (^^) r1 r2 : re = 
  match r1, r2 with 
  | Void, _ -> Void 
  | _, Void -> Void 
  | _, Empty -> r1
  | Empty, _ -> r2
  | _, _ -> Cat (r1, r2)

(** Smart constructor for [star]. Note that:
    - Iterating the empty string gives the empty string, 
    - Zero or more occurrences of [Void] is empty
    - Two iterations is the same as one, i.e. [star (Star r) = Star r] *)  
let star (re : re) : re = 
  match re with 
  | Void | Empty -> Empty 
  | Star re' -> Star re' 
  | _ -> Star re

(** Example regex, where [ex1 = a(b* + c)] *)
let ex1 : re = Char 'a' ^^ (Star (Char 'b') <|> Char 'c')

(** [nullable r] returns [true] when [r] can match the empty string *)
let rec nullable (re : re) : bool = 
  match re with 
  | Void  | Char _ -> false 
  | Empty | Star _ -> true 
  | Alt (r1, r2) -> nullable r1 || nullable r2 
  | Cat (r1, r2) -> nullable r1 && nullable r2

(** Brozozowski derivative *)
let rec deriv (re : re) (c : char) : re = 
  let open Char in 
  match re with 
  | Void | Empty -> Void 
  | Char c' when c = c' -> Empty 
  | Char _ -> Void
  | Alt (r1, r2) -> deriv r1 c <|> deriv r2 c
  | Cat (r1, r2) -> (deriv r1 c ^^ r2) 
                    <|> if nullable r1 then deriv r2 c else Void 
  | Star r -> deriv r c ^^ Star r
  

(** [match r s] determines whether the regex [r] matches the string [s] *)  
let matchRegex (r : re) (s : string) : bool = 
  nullable (String.fold s ~f:deriv ~init:r )


