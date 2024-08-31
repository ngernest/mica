(** Adapted from implementations by:  
    - Colin Shaw (2017)
    https://github.com/CompScienceClub/ocaml-red-black-trees 
    - Benedikt Meurer (2017)
    https://github.com/bmeurer/ocaml-rbtrees/blob/master/src/rbset.ml 
    - CS 3110 textbook, chapter 8.3
    https://cs3110.github.io/textbook/chapters/ds/rb.html#id1

    The implementation of red-black tree deletion follows
    the approach taken by Germane & Might's (2014) functional pearl
    {i Deletion: The curse of the red-black tree}. *)
open Map_signature

(** The color of each red-black tree node. 
        - Note that [BB] represents the {i double-black} transitory color, 
       introduced in Germane & Might's (2014) implementation of red-black tree deletion *)
type color = Red | Black | BB

(** The type of (key, value) bindings stored at each red-black tree node *)
type data = { key : Base.Int.t; value : Base.String.t }

(** The type of red-black trees *)
type rbtree = Empty of color | Node of color * rbtree * data * rbtree

(** {1 Auxiliary functions for deletion from red-black trees} *)
let rec min (t : rbtree) : 'a =
  match t with
  | Empty _ -> failwith "min error"
  | Node (_, Empty _, x, _) -> x
  | Node (_, l, _, _) -> min l

let node_val (t : rbtree) : 'a =
  match t with
  | Empty _ -> failwith "node_val error"
  | Node (_, _, x, _) -> x

let left (t : rbtree) : rbtree =
  match t with
  | Empty _ -> failwith "left error"
  | Node (_, l, _, _) -> l

let right (t : rbtree) : rbtree =
  match t with
  | Empty _ -> failwith "right error"
  | Node (_, _, _, r) -> r

let add_b (t : rbtree) : rbtree =
  match t with
  | Empty Black -> Empty BB
  | Node (Red, l, x, r) -> Node (Black, l, x, r)
  | Node (Black, l, x, r) -> Node (BB, l, x, r)
  | _ -> failwith "add_b error"

let rem_b (t : rbtree) : rbtree =
  match t with
  | Empty BB -> Empty Black
  | Node (BB, l, x, r) -> Node (Black, l, x, r)
  | _ -> failwith "rem_b error"

let is_black (t : rbtree) : bool =
  match t with
  | Empty Black | Node (Black, _, _, _) -> true
  | _ -> false

let is_r (t : rbtree) : bool =
  match t with
  | Node (Red, _, _, _) -> true
  | _ -> false

let is_bb (t : rbtree) : bool =
  match t with
  | Empty BB | Node (BB, _, _, _) -> true
  | _ -> false

let rec bal_del_l (t : rbtree) : rbtree =
  match t with
  | Node (Black, d, y, Node (Red, l, z, r)) ->
    if is_bb d then Node (Black, bal_del_l (Node (Red, d, y, l)), z, r)
    else Node (Black, d, y, Node (Red, l, z, r))
  | Node (c, d, y, Node (Black, l, z, r)) ->
    if is_bb d then
      if is_black l && is_black r then
        add_b (Node (c, rem_b d, y, Node (Red, l, z, r)))
      else if is_r l && is_black r then
        bal_del_l
          (Node
             ( c,
               d,
               y,
               Node (Black, left l, node_val l, Node (Red, right l, z, r)) ))
      else Node (c, Node (Black, rem_b d, y, l), z, add_b r)
    else Node (c, d, y, Node (Black, l, z, r))
  | n -> n

let rec bal_del_r (t : rbtree) : rbtree =
  match t with
  | Node (Black, Node (Red, l, z, r), y, d) ->
    if is_bb d then Node (Black, l, z, bal_del_r (Node (Red, r, y, d)))
    else Node (Black, Node (Red, l, z, r), y, d)
  | Node (c, Node (Black, l, z, r), y, d) ->
    if is_bb d then
      if is_black l && is_black r then
        add_b (Node (c, Node (Red, l, z, r), y, rem_b d))
      else if is_black l && is_r r then
        bal_del_r
          (Node
             ( c,
               Node (Black, Node (Red, l, z, left r), node_val r, right r),
               y,
               d ))
      else Node (c, add_b l, z, Node (Black, r, y, rem_b d))
    else Node (c, Node (Black, l, z, r), y, d)
  | n -> n

(** {1 Module for maps implemented using red-black trees} *)

(** Implementation of maps with (int, string) key-value pairs, 
        implemented using red-black trees *)
module RedBlackMap : S = struct
  type t = rbtree

  let empty = Empty Black

  (** [find k m] is [Some v] if [k] is bound to [v] in [m], 
          and [None] if not. Efficiency: O(n) *)
  let rec find (x : int) (t : rbtree) : string option =
    match t with
    | Empty _ -> None
    | Node (_, lt, x', rt) ->
      if x'.key < x then find x rt
      else if x'.key > x then find x lt
      else Some x'.value

  (** [balance (c, l, v, r)] implements the 4 possible rotations
          for balancing a red-black tree. *)
  let balance ((color, lt, data, rt) : color * rbtree * data * rbtree) : rbtree
      =
    match (color, lt, data, rt) with
    | Black, Node (Red, Node (Red, a, x, b), y, c), z, d (* 1 *)
    | Black, Node (Red, a, x, Node (Red, b, y, c)), z, d (* 2 *)
    | Black, a, x, Node (Red, Node (Red, b, y, c), z, d) (* 3 *)
    | Black, a, x, Node (Red, b, y, Node (Red, c, z, d)) (* 4 *) ->
      Node (Red, Node (Black, a, x, b), y, Node (Black, c, z, d))
    | t -> Node (color, lt, data, rt)

  (** Helper function for [insert], which is the same as BST insertion. 
          - Efficiency: O(log n) *)
  let rec insert_aux (binding : data) (tree : rbtree) : rbtree =
    match tree with
    (* Always color new nodes red, even if it violates the local invariant *)
    | Empty c -> Node (Red, Empty c, binding, Empty c)
    | Node (c, l, d, r) ->
      (* Same as BST insertion, but also call [balance] to restore the local
         invariant *)
      if binding.key < d.key then balance (c, insert_aux binding l, d, r)
      else if binding.key > d.key then balance (c, l, d, insert_aux binding r)
      else Node (c, l, binding, r)

  (** [insert (key, value) tree] inserts [x] into the red-black tree [t], 
      calling [insert_aux] to perform the insertion and 
      re-coloring the root black. Efficiency: O(log n) 
      - If a binding for [key] already exists in the tree, the binding 
        for [key] is updated so that it is mapped to [value]. *)
  let insert ((key, value) : int * string) (tree : rbtree) : rbtree =
    let binding = { key; value } in
    match insert_aux binding tree with
    | Empty _ ->
      failwith "impossible" (* [insert_aux] can never return [Leaf] *)
    | Node (_, l, v, r) -> Node (Black, l, v, r)
  (* Always color the root black *)

  let rec remove_aux (t : rbtree) (x : 'a) : rbtree =
    let rec remove_aux_int (t : rbtree) : rbtree =
      match t with
      | Empty _ -> t
      | Node (Red, Empty _, x', Empty _) ->
        if x'.key = x.key then Empty Black else t
      | Node (Black, Empty _, x', Empty _) ->
        if x'.key = x.key then Empty BB else t
      | Node (_, Empty _, x', Node (_, l, y', r))
      | Node (_, Node (_, l, y', r), x', Empty _) ->
        if x'.key = x.key then Node (Black, l, y', r)
        else if y'.key = x.key then Node (Black, Empty Black, x', Empty Black)
        else t
      | Node (c, l, x', r) ->
        if x'.key < x.key then bal_del_r (Node (c, l, x', remove_aux_int r))
        else if x'.key > x.key then
          bal_del_l (Node (c, remove_aux_int l, x', r))
        else
          let m = min r in
          bal_del_r (Node (c, l, m, remove_aux r m)) in
    remove_aux_int t

  (** [remove k lst] removes {i all} bindings for [k] in [lst]. 
          - Efficiency: O(n). *)
  let remove (x : int) (t : rbtree) : rbtree =
    let binding = { key = x; value = "" } in
    match remove_aux t binding with
    | Empty _ -> Empty Black
    | Node (_, l, x', r) -> Node (Black, l, x', r)

  (** [bindings m] is an association list containing the same bindings 
      as [m], with the (integer-valued) keys sorted in ascending order. 
      - There are no duplicate keys in the association list. *)
  let bindings (tree : rbtree) : AssocList.t =
    let rec bindings_aux acc tree =
      match tree with
      | Empty _ -> acc
      | Node (c, lt, x, rt) ->
        bindings_aux ((x.key, x.value) :: bindings_aux acc rt) lt in
    bindings_aux [] tree

  (** [from_list lst] is a map containing the same bindings as the 
      association list [lst]. 
      Requirement: [lst] does not contain any duplicate keys. *)
  let from_list (lst : AssocList.t) : rbtree =
    List.fold_left (fun acc (k, v) -> insert (k, v) acc) empty lst
end
