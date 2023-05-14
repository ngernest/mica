module type StackInterface = sig
  type 'a t
    [@@deriving sexp]
  val empty : 'a t
  val push : 'a -> 'a t -> 'a t
  val pop : 'a t -> 'a t
  val peek : 'a t -> 'a
  val clear : 'a t -> unit 
  val is_empty : 'a t -> bool
  val length : 'a t -> int 
  val create : unit -> 'a t
end