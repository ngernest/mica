
(* Suppress "unused functor parameter" compiler warning *)
[@@@ocaml.warning "-67"]

module type Field = sig
  type t
  val zero: t

  val ( + ): t -> t -> t
  val ( - ): t -> t -> t
  val double: t -> t

  val one: t

  val ( * ): t -> t -> t
  val ( / ): t -> t -> t
  val square: t -> t
end

module type Integral = sig
  type t
  val zero: t
  val one: t

  val ( + ): t -> t -> t

  val ( mod ): t -> t -> t
  val ( asr ): t -> int -> t
  val logxor: t -> t -> t
  val gt: t -> t -> bool
end

module type Edwards = sig
  type integral
  type element

  val bits: int
  val a24: element

  val constant_time_conditional_swap: integral -> element -> element -> element * element
end

module Make: functor (F: Field)(I: Integral)(E: Edwards with type integral = I.t and type element = F.t) -> sig
  val scale: I.t -> F.t -> F.t
end
