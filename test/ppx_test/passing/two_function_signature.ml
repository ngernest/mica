module type S = sig
  type 'a t

  val f : 'a t -> 'a t
  val g : int -> 'a t
end
