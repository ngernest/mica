module type RegexMatcherIntf = sig
  type t
    [@@deriving sexp]
  val void : t
  val empty : t
  val matchString : t -> string -> bool
  val acceptsEmpty : t -> bool
  val lit : char -> t
  val alt : t -> t -> t
  val cat : t -> t -> t
  val star : t -> t
end
