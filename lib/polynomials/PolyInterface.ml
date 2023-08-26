module type PolyInterface = sig 
  type t
    [@@deriving sexp]
  val zero : t
  val one : t
  val power : int -> int -> int 
  val monomial : int -> int -> t
  val add : t -> t -> t
  val mult : t -> t -> t
  val create : (int * int) list -> t
  val eval : t -> int -> int
  val equal : t -> t -> bool
end