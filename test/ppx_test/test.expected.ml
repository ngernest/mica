module type S = sig
  type 'a t

  val f : 'a t -> 'a t
end
[@@deriving mica_types]

include struct
  type expr = F of expr
  type ty = IntT
end [@@ocaml.doc "@inline"] [@@merlin.hide]
