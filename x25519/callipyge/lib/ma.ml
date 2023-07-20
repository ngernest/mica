type t = int array

external make : int -> int -> t = "caml_make_vect"

external get : t -> int -> int = "%array_unsafe_get"
external set : t -> int -> int -> unit = "%array_unsafe_set"

let init l f =
  let r = make l (f 0) in

  for i = 1 to pred l
  do set r i (f i) done; r

external length : t -> int = "%array_length"

let exists f a = Array.exists f a
let for_all f a = Array.for_all f a
