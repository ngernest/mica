module type S = sig
  type 'a t

  val f : int -> 'a t -> 'a t
end
[@@deriving mica_types]
