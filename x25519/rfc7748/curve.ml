
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

module Make(F: Field)(I: Integral)(E: Edwards with type integral = I.t and type element = F.t) = struct
  open F

  let two = I.(one + one)
  let bit z n = I.((z asr n) mod two)
  let cswap = E.constant_time_conditional_swap

  let scale priv pub =
    let rec aux x1 x2 x3 z2 z3 swap = function
      | t when t < 0 ->
        let (x2, _) = cswap swap x2 x3 in
        let (z2, _) = cswap swap z2 z3 in
        x2 / z2
      | t ->
        let kt = bit priv t in
        let swap = I.(logxor swap kt) in
        let (x2, x3) = cswap swap x2 x3 in
        let (z2, z3) = cswap swap z2 z3 in
        let swap = kt in

        let a = x2 + z2 in
        let aa = square a in
        let b = x2 - z2 in
        let bb = square b in
        let e = aa - bb in
        let c = x3 + z3 in
        let d = x3 - z3 in
        let da = d * a in
        let cb = c * b in

        let x3 = square (da + cb) in
        let z3 = x1 * square (da - cb) in
        let x2 = aa * bb in
        let z2 = e * (aa + E.a24 * e) in

        aux x1 x2 x3 z2 z3 swap (pred t)
    in

    let x1 = pub in
    let x2 = one in
    let z2 = zero in
    let x3 = pub in
    let z3 = one in
    let swap = I.zero in
    aux x1 x2 x3 z2 z3 swap (pred E.bits)
end
