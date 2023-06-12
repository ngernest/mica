open Base

(* Disable "unused-values" compiler warnings *)
[@@@ocaml.warning "-32-34-27"]

module type MapInterface = sig 
  type assoc_list = (int * string) list 

  type t
    [@@deriving sexp]
  val empty : t
  val insert : int * string -> t -> t
  val find : int -> t -> string option 
  val remove : int -> t -> t
  val from_list : assoc_list -> t
  val bindings : t -> assoc_list
end 




