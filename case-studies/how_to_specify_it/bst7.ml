open Bst

module BST7 : BST = struct
  let rec toList t =
    match t with
    | Leaf -> []
    | Branch (l, k, v, r) -> toList l @ [ (k, v) ] @ toList r

  let keys t = List.map fst (toList t)

  let rec valid t =
    match t with
    | Leaf -> true
    | Branch (l, k, _, r) ->
      valid l && valid r
      && List.for_all (fun x -> x < k) (keys l)
      && List.for_all (fun x -> x > k) (keys r)

  let nil = Leaf

  let rec find k t =
    match t with
    | Leaf -> None
    | Branch (l, k', v, r) when k < k' -> find k l
    | Branch (l, k', v, r) when k > k' -> find k r
    | Branch (_, _, v, _) -> Some v

  let size t = List.length (keys t)

  let rec insert k v t =
    match t with
    | Leaf -> branch Leaf k v Leaf
    | Branch (l, k', v', r) when k < k' -> branch (insert k v l) k' v' r
    | Branch (l, k', v', r) when k > k' -> branch l k' v' (insert k v r)
    | Branch (l, k', v', r) -> branch l k' v r

  let rec delete k t =
    match t with
    | Leaf -> Leaf
    | Branch (l, k', v', r) when k < k' -> branch (delete k l) k' v' r
    | Branch (l, k', v', r) when k > k' -> branch l k' v' (delete k r)
    | Branch (l, k', v', r) -> join l r

  and join l r =
    match (l, r) with
    | Leaf, _ -> r
    | _, Leaf -> l
    | Branch (l, k, v, r), Branch (l', k', v', r') ->
      branch l k v (branch (join r l') k' v' r')

  (** Bug 7: [union] wrongly assumes that if the key at the root of [t] is 
      smaller than the key at the root of [t'], then all the keys in [t] will 
      be smaller than the key at the root of [t']. *)
  let rec union l r =
    match (l, r) with
    | Leaf, _ -> r
    | _, Leaf -> l
    | Branch (l, k, v, r), Branch (l', k', v', r') when k = k' ->
      branch (union l l') k v (union r r')
    | Branch (l, k, v, r), Branch (l', k', v', r') when k < k' ->
      branch l k v (branch (union r l') k' v' r)
    | Branch (l, k, v, r), Branch (l', k', v', r') ->
      union (branch l' k' v' r) (branch l k v r)

  let rec insertions t =
    match t with
    | Leaf -> []
    | Branch (l, k, v, r) -> ((k, v) :: insertions l) @ insertions r
end
