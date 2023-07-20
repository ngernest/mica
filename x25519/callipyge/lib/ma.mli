type t = int array

val init: int -> (int -> int) -> t
val make: int -> int -> t

val get: t -> int -> int
val set: t -> int -> int -> unit

val length: t -> int

val exists: (int -> bool) -> t -> bool
val for_all: (int -> bool) -> t -> bool
