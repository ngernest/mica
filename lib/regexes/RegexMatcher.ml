(** An interface for regular-expression matchers. 
    To test Mica with the regex modules, please run Mica on [RegexMatcher.mli], 
    {i not} this file. 
    (The Mica parser currently does not support parsing of comments.)
*)
module type RegexMatcher = sig
  type t [@@deriving sexp]
  (** Abstract type of regexes *)

  val void : t
  (** [void] is the regex that always fails *)

  val empty : t
  (** [empty] is the regex that accepts the empty string *)

  val lit : char -> t
  (** [lit c] constructs a regex that matches the character [c] *)

  val alt : t -> t -> t
  (** Smart constructor for alternation *)

  val cat : t -> t -> t
  (** Smart constructor for concatenation *)

  val star : t -> t
  (** Smart constructor for Kleene star *)

  val matchString : t -> string -> bool
  (** [matchString s re] is [true] if the regex [re] matches the string [s] *)

  val acceptsEmpty : t -> bool
  (** [acceptsEmpty re] is [true] if [re] accepts the empty string *)
end
