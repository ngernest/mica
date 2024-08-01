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

module Mica = struct
  include struct
    type expr =
      | Empty
      | Is_empty of expr
      | Mem of int * expr
      | Add of int * expr
      | Rem of int * expr
      | Size of expr
      | Union of expr * expr
      | Intersect of expr * expr
    [@@deriving show { with_path = false }]

    type ty = Bool | Int | IntT [@@deriving show { with_path = false }]

    let rec gen_expr ty =
      let open Core in
      let open Quickcheck.Generator in
      let open Let_syntax in
      size >>= fun k ->
      match ty with
      | Bool ->
        let gen_is_empty =
          let g__001_ = with_size ~size:(k / 2) (gen_expr IntT) in
          g__001_ >>| fun e__002_ -> Is_empty e__002_
        and gen_mem =
          let g__003_ = quickcheck_generator_int
          and g__004_ = with_size ~size:(k / 2) (gen_expr IntT) in
          tuple2 g__003_ g__004_ >>| fun (e__005_, e__006_) ->
          Mem (e__005_, e__006_) in
        union [ gen_is_empty; gen_mem ]
      | Int ->
        let gen_size =
          let g__007_ = with_size ~size:(k / 2) (gen_expr IntT) in
          g__007_ >>| fun e__008_ -> Size e__008_ in
        union [ gen_size ]
      | IntT ->
        let gen_empty = return Empty
        and gen_add =
          let g__009_ = quickcheck_generator_int
          and g__010_ = with_size ~size:(k / 2) (gen_expr IntT) in
          tuple2 g__009_ g__010_ >>| fun (e__011_, e__012_) ->
          Add (e__011_, e__012_)
        and gen_rem =
          let g__013_ = quickcheck_generator_int
          and g__014_ = with_size ~size:(k / 2) (gen_expr IntT) in
          tuple2 g__013_ g__014_ >>| fun (e__015_, e__016_) ->
          Rem (e__015_, e__016_)
        and gen_union =
          let g__017_ = with_size ~size:(k / 2) (gen_expr IntT)
          and g__018_ = with_size ~size:(k / 2) (gen_expr IntT) in
          tuple2 g__017_ g__018_ >>| fun (e__019_, e__020_) ->
          Union (e__019_, e__020_)
        and gen_intersect =
          let g__021_ = with_size ~size:(k / 2) (gen_expr IntT)
          and g__022_ = with_size ~size:(k / 2) (gen_expr IntT) in
          tuple2 g__021_ g__022_ >>| fun (e__023_, e__024_) ->
          Intersect (e__023_, e__024_) in
        union [ gen_empty; gen_add; gen_rem; gen_union; gen_intersect ]

    let _ = gen_expr
  end

  module Interpret (M : S) = struct
    open M

    type value = ValBool of bool | ValInt of int | ValIntT of int t

    let rec interp e =
      match e with
      | Empty -> ValIntT M.empty
      | Is_empty expr__025_ -> (
        match interp expr__025_ with
        | ValIntT expr__025_' -> ValBool (M.is_empty expr__025_')
        | _ -> failwith "impossible: unary constructor")
      | Mem (int__026_, expr__027_) -> (
        match interp expr__027_ with
        | ValIntT expr__027_' -> ValBool (M.mem int__026_ expr__027_')
        | _ -> failwith "impossible: n-ary constructor")
      | Add (int__028_, expr__029_) -> (
        match interp expr__029_ with
        | ValIntT expr__029_' -> ValIntT (M.add int__028_ expr__029_')
        | _ -> failwith "impossible: n-ary constructor")
      | Rem (int__030_, expr__031_) -> (
        match interp expr__031_ with
        | ValIntT expr__031_' -> ValIntT (M.rem int__030_ expr__031_')
        | _ -> failwith "impossible: n-ary constructor")
      | Size expr__032_ -> (
        match interp expr__032_ with
        | ValIntT expr__032_' -> ValInt (M.size expr__032_')
        | _ -> failwith "impossible: unary constructor")
      | Union (expr__033_, expr__034_) -> (
        match (interp expr__033_, interp expr__034_) with
        | ValIntT expr__033_', ValIntT expr__034_' ->
          ValIntT (M.union expr__033_' expr__034_')
        | _ -> failwith "impossible: n-ary constructor")
      | Intersect (expr__035_, expr__036_) -> (
        match (interp expr__035_, interp expr__036_) with
        | ValIntT expr__035_', ValIntT expr__036_' ->
          ValIntT (M.intersect expr__035_' expr__036_')
        | _ -> failwith "impossible: n-ary constructor")

    let _ = interp
  end

  module TestHarness (M1 : S) (M2 : S) = struct
    module I1 = Interpret (M1)
    module I2 = Interpret (M2)
    open Core

    let test_bool () : unit =
      Quickcheck.test (gen_expr Bool) ~f:(fun e ->
          match (I1.interp e, I2.interp e) with
          | ValBool b1, ValBool b2 -> [%test_eq: bool] b1 b2
          | _ -> failwith "failed bool")

    let test_int () : unit =
      Quickcheck.test (gen_expr Int) ~f:(fun e ->
          match (I1.interp e, I2.interp e) with
          | ValInt i1, ValInt i2 -> [%test_eq: int] i1 i2
          | _ -> failwith "failed int")

    let run_tests () =
      test_bool ();
      test_int ()
  end
end

module Tyche = Json_utils.TycheUtils (struct
  include Mica

  let depth _ = 0
  let num_unique_ints _ = 0
end)

(******************************************************************************)

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
