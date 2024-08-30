open Map_signature
open Base_quickcheck
module G = Generator

(* Disable "unused-values" compiler warnings *)
[@@@ocaml.warning "-32-34-27"]

(** Implementation of maps using association lists.  
    - Adapted from Cornell CS 3110 textbook, chapter 8. *)

module AssocListMap : S = struct
  (** AF: The association list [[(k1, v1); (k2, v2); ...; (kn, vn)]] 
          is the map {k1 : v1, k2 : v2, .., kn : vn}. 
          If a key appears more than once in the list, then in the
          map it is bound to the left-most occurrence in the list. For example,
          [[(k, v1); (k, v2)]] represents {k : v1}. The empty list represents
          the empty map.
          RI: none. *)
  type t = AssocList.t [@@deriving sexp]

  (** Efficiency: O(1). *)
  let insert (k, v) m = (k, v) :: m

  (** [find k m] is [Some v] if [k] is bound to [v] in [m], 
      and [None] if not. Efficiency: O(n) *)
  let find = List.assoc_opt

  (** [remove k lst] removes {i all} bindings for [k] in [lst]. 
      Efficiency: O(n). *)
  let remove k lst = List.filter (fun (k', _) -> k <> k') lst

  (** The empty map is represented by the empty list. *)
  let empty = []

  (** [from_list lst] is a map containing the same bindings as the 
      association list [lst]. 
      Requirement: [lst] does not contain any duplicate keys. *)
  let from_list (lst : AssocList.t) : AssocList.t = lst

  (** [keys m] is a list of the keys in [m], without any duplicates.
      This function performs the following:
      - Gets a list of keys
      - Remove duplicates
      - Create pairs of remaining keys and values 
      Efficiency: O(n log n). *)
  let keys (m : ('a * 'b) list) : 'a list =
    m |> List.map fst |> List.sort_uniq Stdlib.compare

  (** [binding m k] is [(k, v)], where [v] is the value that [k] binds in [m].
      Requires: [k] is a key in [m].
      Efficiency: O(n). *)
  let binding (m : AssocList.t) (k : int) : int * string = (k, List.assoc k m)

  (** [bindings m] is an association list containing the same bindings 
      as [m]. There are no duplicate keys in the list. 
      Efficiency: O(n log n) + O(n) * O(n), which is O(n^2). 
      (In the worst-case, there are n keys for [m].) *)
  let bindings (m : AssocList.t) : AssocList.t = List.map (binding m) (keys m)
end

(** {1 Utility functions} *)

(** [cmp_set_like_lists lst1 lst2] compares two association lists 
    to see whether they are equivalent {i set-like} lists. This means:
    - The two lists can't contain any duplicates
    - The two lists must contain the same elements though 
      not necessarily in the same order *)
let cmp_set_like_lists lst1 lst2 =
  let uniq1 = List.sort_uniq compare lst1 in
  let uniq2 = List.sort_uniq compare lst2 in
  List.length lst1 = List.length uniq1
  && List.length lst2 = List.length uniq2
  && uniq1 = uniq2

(** [pp_string s] pretty-prints the string [s] *)
let pp_string s = "\"" ^ s ^ "\""

(** [pp_list pp_elt lst] pretty-prints the list [lst], 
    using [pp_elt] to pretty-print each element of [lst] *)
let pp_list (pp_elt : 'a -> string) (lst : 'a list) : string =
  (* [pp_elts] is the pretty-printed list *)
  let pp_elts (lst : 'a list) : string =
    (* Walks over the list, using [pp_elt] to print up to 100 elements *)
    let rec loop (n : int) (acc : string) (l : 'a list) : string =
      match l with
      | [] -> acc
      | [ h ] -> acc ^ pp_elt h
      | h1 :: (_ :: _ as t') ->
        if n = 100 then acc ^ "..." (* stop printing *)
        else loop (n + 1) (acc ^ pp_elt h1 ^ "; ") t' in
    loop 0 "" lst in
  "[" ^ pp_elts lst ^ "]"

(** [pp_pair pp1 pp2 (a, b)] pretty-prints [(a, b)] 
    using [pp1] for [a] and [pp2] for [b] *)
let pp_pair pp1 pp2 (a, b) = pp1 a ^ pp2 b
