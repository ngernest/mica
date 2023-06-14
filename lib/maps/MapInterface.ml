module type MapInterface = sig 
  type t
    [@@deriving sexp]
  type assoc_list = (int * string) list
  val empty : t
  val insert : int * string -> t -> t
  val find : int -> t -> string option 
  val remove : int -> t -> t
  val from_list : assoc_list -> t
  val bindings : t -> assoc_list
end 




