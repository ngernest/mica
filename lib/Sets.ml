(** Adapted from:

    Cornell CS 3110 textbook Chapters 6.3 & 8.3
    https://cs3110.github.io/textbook/chapters/correctness/module_docs.html 
    https://cs3110.github.io/textbook/chapters/ds/rb.html#maps-and-sets-from-bsts
    
    Ilya Sergey's OCaml Data Structures notes
    https://ilyasergey.net/YSC2229/week-11-bst.html

    Penn CIS 1200 HW3
    https://www.seas.upenn.edu/~cis120/23sp/hw/hw03/

*)

open TestHarness

(** A set is an unordered collection in which multiplicity is ignored. *)
module type SetIntf = sig

  (** ['a t] represents a set whose elements are of type ['a] *)
  type 'a t

  (** [empty] is the set containing no elements *)
  val empty : 'a t

  (** Checks if the set is currently empty *)
  val is_empty : 'a t -> bool 

  (** [mem x s] is whether [x] is a member of set [s] *)
  val mem : 'a -> 'a t -> bool

  (** [add x s] is the set containing all the elements of [s]
      as well as [x]. *)
  val add : 'a -> 'a t -> 'a t

  (** [rem x s] is the set containing all the elements of [s],
      minus [x]. *)
  val rem : 'a -> 'a t -> 'a t

  (** [size s] is the cardinality of [s] *)
  val size: 'a t -> int

  (** [union s1 s2] is the set containing all the elements that
      are in either [s1] or [s2]. *)
  val union: 'a t -> 'a t -> 'a t

  (** [inter s1 s2] is the set containing all the elements that
      are in both [s1] and [s2]. *)
  val inter: 'a t -> 'a t -> 'a t

  (** [string f s] is a representation of [s] as a string, using [f] 
      to represent elements as strings. *)
  val string: ('a -> string) -> 'a t -> string 
  
  (** [rep_ok s] checks if the representation invariant holds for [s],
      and throws an exception if the RI doesn't hold *)
  val rep_ok : 'a t -> 'a t 
end

(** [dedup lst] is [lst] but with duplicates removed. 
    It also sorts the output list. *)
let dedup lst = 
    lst |> List.sort_uniq Stdlib.compare

(** Converts the interior elments of a list ot a stirng*)    
let interior string_of_elt h t = 
    t
    |> List.map string_of_elt
    |> List.fold_left (fun acc elt -> acc ^ ", " ^ elt) (string_of_elt h)

(** Converts a list to the string representation of a set *)    
let string_of_list string_of_elt s = 
    match s with 
      | [] -> "{}"
      | h :: t -> "{" ^ interior string_of_elt h t ^ "}"

(** Implementation of sets as lists with duplicates *)
module ListSetDups : SetIntf = struct
    (** Abstraction function (AF): The list [a1; ...; an] represents the set {b1, ..., bm},
        where [b1; ...; bm] is the same list as [a1; ...; an] but with duplicates removed. 
        The empty list [[]] represents the empty set [{}].
        
        Representation invariant (RI): None: The list _may_ contain duplicates. *)
    type 'a t = 'a list
    
    let empty = []

    let is_empty s = s == []

    let mem = List.mem
    
    let add = List.cons
    
    let rem x = List.filter (( <> ) x)
    
    let size lst = lst |> List.sort_uniq Stdlib.compare |> List.length
    
    let union = List.append

    let inter lst1 lst2 = List.filter (fun h -> mem h lst2) lst1

    let string f s = 
        s |> dedup |> string_of_list f

    (* No RI for representing sets as lists w/ duplicates, so [rep_ok] is trivial *)
    let rep_ok s = s
end



(** Implementation of sets as lists without duplicates *)
module ListSetNoDups : SetIntf = struct 
    (** AF: The list [a1; ...; an] represents the set {a1, ..., an}. 
            The empty list [[]] represents the empty set. 
        RI: The list must not contain duplicates. *)
    type 'a t = 'a list

    (** [rep_ok] checks the representation invariant. 
        Restore the (expensive) implementation of [rep_ok] when checking *)
    let rep_ok lst = lst
        (* if List.length lst = List.length (dedup lst) then lst 
        else failwith "RI" *)

    let empty = rep_ok []

    let is_empty lst = (rep_ok lst) == []
    
    let mem x lst = List.mem x (rep_ok lst)
    
    let add x s = 
        let s = rep_ok s in 
        if mem x s then s else rep_ok (x :: s)
    
    let rem x = List.filter (( <> ) x)
    
    let size s = 
        rep_ok s |> dedup |> List.length
    
    let union s1 s2 = 
        (rep_ok s1) @ (rep_ok s2) |> dedup |> rep_ok
    
    let inter lst1 lst2 = rep_ok (List.filter (fun h -> mem h lst2) (rep_ok lst1))

    let string f s = 
        string_of_list f (rep_ok s)
end

(*******************************************************************************)
(** Implementing sets using BSTs *)

(** Datatype for polymorphic binary trees *)
type 'a tree =
  | Empty
  | Node of 'a tree * 'a * 'a tree
  [@@deriving show]

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


module BSTSet : SetIntf = struct
    (* AF: [Leaf] represents the empty set.
          [Node (lt, x, rt)] represents the set containing [x] & 
          all elements in the sets represented by [lt] and [rt] respectively. 
          
       RI: for every [Node (lt, x, rt)], all the values in [lt] are strictly
        less than [x], and all values in [rt] are strictly greater than [x]. *)
    type 'a t = 'a tree
  
    let empty = Empty

    let is_empty t = 
      match t with 
      | Empty -> true 
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
  
    let union = failwith "TODO"

    let inter = failwith "TODO"

    (** [rep_ok t] checks if the BST invariant holds for the tree [t]
        The main recursive sub-function walk works by “growing” a predicate p 
        that applies to each node further down the tree, making sure that it is 
        correctly positioned with regard to all its parents. At the top level p 
        is instantiated with (fun _ -> true), as there are no restrictions 
        imposed for the root of the tree, but more and more conjuncts added, 
        as the checking proceeds recursively. *)
    let rep_ok t = 
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
        | Empty -> t
        | Node (_, _, _) -> 
            if walk t (fun _ -> true) then t else failwith "RI"

    let string = failwith "TODO"
    

    (* Note that two binary search trees can be equal as sets without having the
       exact same structure. The OCaml (=) operator will only return true if the
       trees look exactly the same, and _not_ if they contain the same elements
       but don't have the same exact structure. The test case below specifies
       the behavior of `equals`. *)
  
    let equals (t1: 'a t) (t2: 'a t) : bool =
        (inorder t1) = (inorder t2)
  
  end
(*******************************************************************************)
(** Functor that returns a test harness 
    comparing two modules that both implement the [Set] signature *)
module CompareSetImpls (A : SetIntf) (B : SetIntf) : Spec2 = struct 
    open! Base
    open! Base_quickcheck

    (** Type of the model's state 
        Note: Set.M(Int).t == (String.t, String.comparator_witness) Set.t *)
    type state = Set.M(Int).t

    (** The respective types of the systems under test (SUTs) *)
    type sutA = int A.t 
    type sutB = int B.t

    (** Generic SUT type *)
    type sut = AImpl of sutA | BImpl of sutB 
    
    (** Symbolic commands for the set ADT *)
    type cmd = 
      | Mem of int 
      | Add of int 
      | Remove of int 
      | Union 
      | Intersection
      | Size 
      | Is_empty 
      [@@deriving sexp_of, quickcheck]

     (** Generate symbolic commands based on the model's current state 
      *  TODO: figure out what to do about binary operations eg. [Union] *)
     let gen_cmd (st : state) : cmd Generator.t = 
      let module G = Generator in 
        G.union @@ (
          if Set.is_empty st then []
          else [G.return Union;
                G.return Intersection])
          @
        [G.map ~f:(fun x -> Mem x) G.int;
          G.map ~f:(fun x -> Add x) G.int; 
          G.map ~f:(fun x -> Remove x) G.int;
          G.return Is_empty;
          G.return Size]

    let init_state = Set.empty (module Int)

    let init_sutA () = A.empty 
    let init_sutB () = B.empty 

    (** Given a command [cmd] and the current state [st], move 
      the model's state to the next state by interpreting [cmd] *)
    let next_state (cmd : cmd) (s : state) : state = 
      match cmd with 
      | Add x -> Set.add s x
      | Remove x -> Set.remove s x
      | Mem _ | Is_empty | Size -> s
      | Union | Intersection -> failwith "TODO: handle binary operations"

    (** Interprets the symbolic command [cmd] over the two SUTs,
        & checks whether the result of interpreting [cmd] over [sutA] 
        & [sutB] agree with each other. 
        Here, [st] refers to the model's state prior to executing [cmd]. *)
    let compare_cmd (cmd : cmd) (st : state) (sutA : sutA) (sutB : sutB): bool = 
      (* Use polymorphic comparison for simplicity *)
      let open Base.Poly in 
      match cmd with 
      | Size -> 
        let (sa, sb, ss) = (A.size sutA, B.size sutB, Set.length st) in 
          sa = sb && sa = ss && sb = ss
      | Is_empty -> 
        let (ea, eb, es) = (A.is_empty sutA, B.is_empty sutB, Set.is_empty st) in 
          ea = eb && ea = es && eb = es
      | Mem x -> 
        let (memA, memB, memS) = (A.mem x sutA, B.mem x sutB, Set.mem st x) in 
          memA = memB && memA = memS && memB = memS
      | Add x -> 
        let (sa, sb, ss) = (A.(add x sutA |> size), B.(add x sutB |> size), Set.(add st x |> length)) in 
          sa = sb && sa = ss && sb = ss
      (* TODO: need some way of dealing with exceptions for [remove] *)
      | Remove x -> 
        let (sa, sb, ss) = (A.(rem x sutA |> size), B.(rem x sutB |> size), Set.(remove st x |> length)) in 
          sa = sb && sa = ss && sb = ss
      | _ -> failwith "TODO"

    let precond (cmd : cmd) (st : state) : bool = 
    match cmd with
    | Mem _ | Remove _ -> not (Set.is_empty st)
    | Add _ | Is_empty | Size -> true
    | Union | Intersection -> failwith "TODO"
    

    (* Nothing to do for cleanup *)
    let cleanupA _ = ()
    let cleanupB _ = ()

end 
