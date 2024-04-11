open QueueIntf

module Queue : QueueIntf with type 'a t := 'a Base.Queue.t = struct
  open Base.Queue

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
