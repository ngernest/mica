open QueueIntf

module Linked_queue : QueueIntf with type 'a t := 'a Base.Linked_queue.t =
struct
  open Base.Linked_queue

  let sexp_of_t = sexp_of_t
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
