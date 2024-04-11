module type QueueIntf = sig
  type 'a t

  val create : unit -> int t
  val length : int t -> int
  val is_empty : int t -> bool
  val mem : int t -> int -> bool
  val to_list : int t -> int list
  val enqueue : int t -> int -> unit
  val dequeue : int t -> int option
  val peek : int t -> int option
  val min_elt : int t -> int option
end
