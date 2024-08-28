(** A module signature for persistent queues *)
module type S = sig
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

(** Implementation of persistent queues using [Base.Queue] *)
module Queue : S = struct
  open Base.Queue

  type 'a t = 'a Base.Queue.t

  let create = create ~capacity:100
  let length = length
  let is_empty = is_empty
  let mem = mem ~equal:Int.equal
  let to_list = to_list
  let enqueue = enqueue
  let dequeue = dequeue
  let peek = peek
  let min_elt = min_elt ~compare:Int.compare
end

(** Implementation of persistent queues using [Base.Linked_queue] *)
module Linked_queue : S = struct
  open Base.Linked_queue

  type 'a t = 'a Base.Linked_queue.t

  let create = create
  let length = length
  let is_empty = is_empty
  let mem = mem ~equal:Int.equal
  let to_list = to_list
  let enqueue = enqueue
  let dequeue = dequeue
  let peek = peek
  let min_elt = min_elt ~compare:Int.compare
end
