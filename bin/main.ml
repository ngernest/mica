module type SetInterface = sig
  (* TODO: handle [@@@deriving sexp]*)
  type 'a t 

  val empty : 'a t
  val is_empty : 'a t -> bool
  val mem : 'a -> 'a t -> bool
  val add : 'a -> 'a t -> 'a t
  val rem : 'a -> 'a t -> 'a t
  val size : 'a t -> int
  val union : 'a t -> 'a t -> 'a t
  val intersect : 'a t -> 'a t -> 'a t
  val invariant : 'a t -> bool
end
[@@deriving mica]  


(* 

Currently generated code: 
{[
  type expr = 
    | T of int 
]}

Want:
{[
  type expr = 
    | Empty
]}    

*)



let () = Lib.entrypoint
