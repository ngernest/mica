module type PolyIntf = sig 
  (** Abstract type of the polynomial *)
  type t

  (** Type of coefficients, which come from a ring (eg. integers) *)
  type coeff 

  (** The polynomials f(x) = 0 & f(x) = 1 *)
  val zero : t
  val one : t

  (** [monomial c n] creates the monomial $c * x^n$ *)
  val monomial : coeff -> int -> t

  (** Polynomial Addition & Multiplication *)
  val add : t -> t -> t
  val mul : t -> t -> t

  (** Evaluates a polynomial at a particular point *)
  val eval : t -> coeff -> coeff

  (** Structural equality of polynomials *)
  val equal : t -> t -> bool
end