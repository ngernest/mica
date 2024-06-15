module type BST = sig
  type ('k, 'v) t = Leaf | Branch of ('k, 'v) t * 'k * 'v * ('k, 'v) t

  val find : 'k -> ('k, 'v) t -> 'v option
  val nil : ('k, 'v) t
  val insert : 'k -> 'v -> ('k, 'v) t -> ('k, 'v) t
  val delete : 'k -> ('k, 'v) t -> ('k, 'v) t
  val union : ('k, 'v) t -> ('k, 'v) t -> ('k, 'v) t
  val toList : ('k, 'v) t -> ('k * 'v) list
  val keys : ('k, 'v) t -> 'k list
  val size : ('k, 'v) t -> int
end

(* TODO: add the [[@deriving]] directive to this test case *)
