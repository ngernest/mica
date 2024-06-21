module type S = sig
  type 'a t

  val f : 'a t -> 'a t
end
[@@deriving_inline mica_types]

type expr = F of expr
type ty = IntT

let gen_expr ty = let open Core.Quickcheck.Generator.Let_syntax in "TODO"
let _ = gen_expr

[@@@end]
