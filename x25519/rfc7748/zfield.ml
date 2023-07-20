
module Zp(P: sig val p: Z.t end): Curve.Field with type t = Z.t = struct
  type t = Z.t

  include P

  let zero = Z.zero
  let one = Z.one

  let ( + ) x y = Z.erem (Z.add x y) p
  let ( * ) x y = Z.erem (Z.mul x y) p

  let double x = x + x
  let square x = x * x

  let invert x = Z.invert x p

  let ( / ) x y = x * invert y
  let ( - ) x y = Z.(erem (x - y) p)

end
