module type CharsetSig = sig
  type t

  val empty : t
  val add : char -> t -> t
  val singleton : char -> t
  val remove : char -> t -> t
  val union : t -> t -> t
  val inter : t -> t -> t
  val disjoint : t -> t -> bool
  val diff : t -> t -> t
  val cardinal : t -> int
  val elements : t -> char list
  val min_elt : t -> char
  val min_elt_opt : t -> char option
  val max_elt : t -> char
  val max_elt_opt : t -> char option
  val choose : t -> char
  val choose_opt : t -> char option
  val is_empty : t -> bool
end

module Stdlib_Charset : CharsetSig = Set.Make (Char)
module Yallop_Charset : CharsetSig = Charset
