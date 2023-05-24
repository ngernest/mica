open SetInterface

(** Datatype for polymorphic binary trees *)
type 'a tree =
  | Empty
  | Node of 'a tree * 'a * 'a tree
  [@@deriving sexp]

let leaf x = Node (Empty, x, Empty)

(** [lookup x t] searches the BST [t] for the element [x] & returns a boolean
    [lookup] leverages the BST invariants so we don't have to search every subtree *)
let rec lookup (x: 'a) (t: 'a tree) : bool = 
    match t with 
    | Empty -> false 
    | Node (lt, n, rt) -> 
        if x < n then lookup x lt 
        else if x > n then lookup x rt 
        else (x = n)
  

(** [tree_max t] finds the max element in a binary tree, 
    using the polymorphic [max] function *)        
let rec tree_max (t: 'a tree) : 'a =
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
let rec inorder (t: 'a tree) : 'a list =
    match t with
    | Empty -> []
    | Node (Empty, x, Empty) -> [x]
    | Node (lt, x, Empty) -> (inorder lt) @ [x]
    | Node (Empty, x, rt) -> x::(inorder rt)
    | Node (lt, x, rt) -> (inorder lt) @ (x::(inorder rt))     

(** Prints an int tree to the console *)
let print_tree (t: int tree) : unit =
    let rec w d = if d = 0 then "" else " " ^ w (d - 1) in
    let rec aux t d =
      w (d * 4) ^
      match t with
      | Empty -> "Empty"
      | Node (lt, n, rt) ->
        "Node ( \n" ^
        aux lt (d + 1) ^ ",\n" ^
        w ((d + 1) * 4) ^ (string_of_int n) ^ ",\n" ^
        aux rt (d + 1) ^ "\n" ^
        w (d * 4) ^ ")"
    in print_string @@ "\n" ^ (aux t 0) ^ "\n"

module BSTSet : SetInterface = struct
  (* AF: [Leaf] represents the empty set.
        [Node (lt, x, rt)] represents the set containing [x] & 
        all elements in the sets represented by [lt] and [rt] respectively. 
        
     RI: for every [Node (lt, x, rt)], all the values in [lt] are strictly
      less than [x], and all values in [rt] are strictly greater than [x]. *)
  
  type 'a t = 'a tree
    [@@deriving sexp]

  let empty = Empty

  let is_empty t = 
    match t with 
    | Empty -> false 
    | Node _ -> false 

  (** [add x t] inserts [x] into the BST [t] *)
  let rec add (x: 'a) (t: 'a t) : 'a t =
    match t with 
    | Empty -> Node (Empty, x, Empty)
    | Node (lt, n, rt) ->
      if x < n then Node (add x lt, n, rt)
      else if x > n then Node (lt, n, add x rt)
      else t

  (** [rem x t] removes [x] from the BST [t], returning the resultant BST *)
  let rec rem (x: 'a) (t: 'a t) : 'a t =
      match t with
      | Empty -> Empty
      | Node (lt, n, rt) ->
          if x < n then Node (rem x lt, n, rt)  
          else if x > n then Node (lt, n, rem x rt)
          else match (lt, rt) with
              | (Empty, Empty) -> Empty
              | (lt, Empty) -> lt
              | (Empty, rt) -> rt
              | _ -> let m = tree_max lt in
                  Node (rem m lt, m, rt)
      

  (* Set membership for BSTs is equivalent to the [lookup] function *)
  let mem (x: 'a) (s: 'a t) : bool = lookup x s

  let rec size (t : 'a t) : int =
    match t with 
    | Empty -> 0
    | Node (lt, _, rt) -> size lt + 1 + size rt
      (* size lt + 1 + size rt *)

  (* Union: convert [s2] to a list and do a fold over it, 
     inserting each element into [s1] *)
  let union s1 s2 = 
      let open Base.List in 
      let lst2 = inorder s2 in 
      fold lst2 ~init:s1 ~f:(fun acc x -> add x acc)

  (* Intersection: Extracts the common elements of the two BSTs 
     in a list, and populates a new BST with the common elements *)
  let intersect s1 s2 = 
      let open Base in 
      let lst2 = inorder s2 in 
      let commonElts = List.filter lst2 ~f:(fun x -> mem x s1) in
      List.fold commonElts ~init:empty ~f:(fun acc x -> add x acc)

  (** [invariant t] checks if the BST invariant holds for the tree [t]
      The main recursive sub-function walk works by “growing” a predicate p 
      that applies to each node further down the tree, making sure that it is 
      correctly positioned with regard to all its parents. At the top level p 
      is instantiated with (fun _ -> true), as there are no restrictions 
      imposed for the root of the tree, but more and more conjuncts added, 
      as the checking proceeds recursively. *)
  let invariant t = 
      let rec walk node p =
          match node with 
          | Empty -> true 
          | Node (lt, x, rt) -> 
              let res_left = match lt with
              | Empty -> true
              | Node _ -> walk lt (fun w -> p w && w <= x)
              in
              let res_right = match t with
              | Empty -> true
              | Node _ -> walk rt (fun w -> p w && w >= x)
              in
            (p x && res_left && res_right)
        in
      match t with
      | Empty -> true
      | Node (_, _, _) -> 
          if walk t (fun _ -> true) then true else false
  

  (* Note that two binary search trees can be equal as sets without having the
     exact same structure. The OCaml (=) operator will only return true if the
     trees look exactly the same, and _not_ if they contain the same elements
     but don't have the same exact structure. The test case below specifies
     the behavior of `equals`. *)

  (* let equals (t1: 'a t) (t2: 'a t) : bool =
      (inorder t1) = (inorder t2) *)

end