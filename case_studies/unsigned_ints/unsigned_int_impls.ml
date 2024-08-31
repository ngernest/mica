(** A module signature for an unsigned integer arithmetic library *)
module type S = sig
  type t

  (** Addition. *)
  val add : t -> t -> t

  (** Subtraction. *)
  val sub : t -> t -> t

  (** Multiplication. *)
  val mul : t -> t -> t

  (** The greatest representable integer. *)
  val max_int : t

  (** Bitwise logical and. *)
  val logand : t -> t -> t

  (** Bitwise logical or. *)
  val logor : t -> t -> t

  (** Bitwise logical exclusive or. *)
  val logxor : t -> t -> t

  (** {!shift_left} [x] [y] shifts [x] to the left by [y] bits. *)
  val shift_left : t -> int -> t

  (** {!shift_right} [x] [y] shifts [x] to the right by [y] bits. *)
  val shift_right : t -> int -> t

  (** Convert the given int value to an unsigned integer. *)
  val of_int : int -> t

  (** Convert the given unsigned integer value to an int. *)
  val to_int : t -> int

  (** Convert the given string to an unsigned integer.  Raise {!Failure}
      if the given string is not a valid representation of an unsigned
      integer. *)
  val of_string : string -> t

  (** Return the string representation of its argument. *)
  val to_string : t -> string

  (** The integer 0. *)
  val zero : t

  (** The integer 1. *)
  val one : t

  (** Bitwise logical negation. *)
  val lognot : t -> t

  (** Successor. *)
  val succ : t -> t

  (** Predecessor. *)
  val pred : t -> t

  (** Tests for equality, with the same specification as {!Stdlib.(=)}. *)
  val equal : t -> t -> bool

  (** The comparison function for unsigned integers, with the same
      specification as {!Stdlib.compare}. *)
  val compare : t -> t -> int

  val to_int64 : t -> int64
  val of_int64 : int64 -> t
end

(******************************************************************************)
(* Two implementations of 8-bit unsigned integer arithmetic *)

module U8_1 : S = Unsigned.UInt8

module U8_2 : S = struct
  include Stdint.Uint8

  let equal x y = compare x y = 0
end

(******************************************************************************)
(* Two implementations of 16-bit unsigned integer arithmetic *)

module U16_1 : S = Unsigned.UInt16

module U16_2 : S = struct
  include Stdint.Uint16

  let equal x y = compare x y = 0
end

(******************************************************************************)
(* Two implementations of 32-bit unsigned integer arithmetic *)

module U32_1 : S = Unsigned.UInt32

module U32_2 : S = struct
  include Stdint.Uint32

  let equal x y = compare x y = 0
end

(******************************************************************************)
(* Two implementations of 64-bit unsigned integer arithmetic *)

module U64_1 : S = Unsigned.UInt64

module U64_2 : S = struct
  include Stdint.Uint64

  let equal x y = compare x y = 0
end
