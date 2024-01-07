module type S = sig
  type 'a t

  val f : 'a t -> 'a t
end
[@@deriving_inline mica_types]

type expr = F of expr
type ty = IntT

[@@@end]
