module type S = sig 
  type 'a t 
  val f : 'a t -> 'a t 
  val g : int -> 'a t 
end
[@@deriving_inline mica_types]   

type expr =
  | F of expr
  | G of int
type ty =
  | IntT
[@@@end]
