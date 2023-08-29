module type MapInterface = sig
  type t [@@deriving sexp]

  val empty : t
  val insert : int * string -> t -> t
  val find : int -> t -> string option
  val remove : int -> t -> t
  val from_list : AssocList.t -> t
  val bindings : t -> AssocList.t
end
