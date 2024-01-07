module type S = sig
  type 'a t

  val f : 'a t -> 'a t
end
[@@deriving mica_types]
