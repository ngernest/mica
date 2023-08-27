(** An interface for regular-expression matchers. 
    To test Mica with the regex modules, please run Mica on [RegexMatcher.mli], 
    {i not} this file. 
    (The Mica parser currently does not support parsing of comments.)
*)
module type RegexMatcher = sig 
  (** Abstract type of regexes *)
  type t 
    [@@deriving sexp]

  (** [void] is the regex that always fails *)
  val void : t 

  (** [empty] is the regex that accepts the empty string *)
  val empty : t 

  (** [lit c] constructs a regex that matches the character [c] *)
  val lit : char -> t 

  (** Smart constructor for alternation *)
  val alt : t -> t -> t 

  (** Smart constructor for concatenation *)
  val cat : t -> t -> t

  (** Smart constructor for Kleene star *)
  val star : t -> t 

  (** [matchString s re] is [true] if the regex [re] matches the string [s] *)
  val matchString : t -> string -> bool 
  
  (** [acceptsEmpty re] is [true] if [re] accepts the empty string *)
  val acceptsEmpty : t -> bool 
end  