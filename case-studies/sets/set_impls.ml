(** A module signature for finite sets *)
module type S = sig
  type 'a t

  val empty : 'a t
  val is_empty : 'a t -> bool
  val mem : 'a -> 'a t -> bool
  val add : 'a -> 'a t -> 'a t
  val rem : 'a -> 'a t -> 'a t
  val size : 'a t -> int
  val union : 'a t -> 'a t -> 'a t
  val intersect : 'a t -> 'a t -> 'a t
end

(** Finite sets implemented using lists *)
module ListSet : S = struct
  type 'a t = 'a list

  let empty = []
  let is_empty lst = lst == []
  let mem x lst = List.mem x lst
  let add x s = if mem x s then s else x :: s
  let rem x = List.filter (( <> ) x)
  let dedup lst = lst |> List.sort_uniq Stdlib.compare
  let size s = s |> dedup |> List.length
  let union s1 s2 = s1 @ s2 |> dedup
  let intersect lst1 lst2 = List.filter (fun h -> mem h lst2) lst1
end

(** Finite sets implemented using binary search trees  *)
module BSTSet : S = struct
  type 'a t = Empty | Node of 'a t * 'a * 'a t

  (** [lookup x t] searches the BST [t] for the element [x] & returns a boolean
    [lookup] leverages the BST invariants so we don't have to search every subtree *)
  let rec lookup (x : 'a) (t : 'a t) : bool =
    match t with
    | Empty -> false
    | Node (lt, n, rt) ->
      if x < n then lookup x lt else if x > n then lookup x rt else x = n

  (** [tree_max t] finds the max element in a binary tree, 
    using the polymorphic [max] function *)
  let rec tree_max (t : 'a t) : 'a =
    match t with
    | Empty -> failwith "Tree must be non-empty in order to find max element"
    | Node (Empty, x, Empty) -> x
    | Node (lt, x, Empty) -> max x (tree_max lt)
    | Node (Empty, x, rt) -> max x (tree_max rt)
    | Node (lt, x, rt) ->
      let max_lt = tree_max lt in
      let max_rt = tree_max rt in
      max x (max max_lt max_rt)

  (** [inorder t] performs an inorder traversal of the BST [t] *)
  let rec inorder (t : 'a t) : 'a list =
    match t with
    | Empty -> []
    | Node (Empty, x, Empty) -> [ x ]
    | Node (lt, x, Empty) -> inorder lt @ [ x ]
    | Node (Empty, x, rt) -> x :: inorder rt
    | Node (lt, x, rt) -> inorder lt @ (x :: inorder rt)

  let empty = Empty

  let is_empty t =
    match t with
    | Empty -> true
    | Node _ -> false

  (** [add x t] inserts [x] into the BST [t] *)
  let rec add (x : 'a) (t : 'a t) : 'a t =
    match t with
    | Empty -> Node (Empty, x, Empty)
    | Node (lt, n, rt) ->
      if x < n then Node (add x lt, n, rt)
      else if x > n then Node (lt, n, add x rt)
      else t

  (** [rem x t] removes [x] from the BST [t], returning the resultant BST *)
  let rec rem (x : 'a) (t : 'a t) : 'a t =
    match t with
    | Empty -> Empty
    | Node (lt, n, rt) -> (
      if x < n then Node (rem x lt, n, rt)
      else if x > n then Node (lt, n, rem x rt)
      else
        match (lt, rt) with
        | Empty, Empty -> Empty
        | lt, Empty -> lt
        | Empty, rt -> rt
        | _ ->
          let m = tree_max lt in
          Node (rem m lt, m, rt))

  (* Set membership for BSTs is equivalent to the [lookup] function *)
  let mem (x : 'a) (s : 'a t) : bool = lookup x s

  let rec size (t : 'a t) : int =
    match t with
    | Empty -> 0
    | Node (lt, _, rt) -> size lt + 1 + size rt
  (* size lt + 1 + size rt *)

  (* Union: convert [s2] to a list and do a fold over it, inserting each element
     into [s1] *)
  let union s1 s2 =
    let open Base.List in
    let lst2 = inorder s2 in
    fold lst2 ~init:s1 ~f:(fun acc x -> add x acc)

  (* Intersection: Extracts the common elements of the two BSTs in a list, and
     populates a new BST with the common elements *)
  let intersect s1 s2 =
    let open Base in
    let lst2 = inorder s2 in
    let commonElts = List.filter lst2 ~f:(fun x -> mem x s1) in
    List.fold commonElts ~init:empty ~f:(fun acc x -> add x acc)
end
