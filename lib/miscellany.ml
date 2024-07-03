open Ppxlib
open StdLabels

(** {1 Miscellany} *)

(** Alias for [Stdio.printf] *)
let printf = Stdio.printf

(** Constructs a [loc] given some payload [txt] and a location [loc] *)
let with_loc (txt : 'a) ~(loc : Location.t) : 'a Location.loc = { txt; loc }

(** Strips the location info from a value of type ['a loc] *)
let no_loc (a_loc : 'a Location.loc) : 'a = a_loc.txt

(** Maps a function [f] over a value of type ['a loc], 
    returning a value of type o[b loc] *)
let map_with_loc ~(f : 'a -> 'b) (alpha : 'a Location.loc) : 'b Location.loc =
  let a = alpha.txt in
  { alpha with txt = f a }

(** Takes the maximum of two [Location.t] values using Ppxlib's in-built 
    [Location.compare] function *)
let max_loc (l1 : Location.t) (l2 : Location.t) : Location.t =
  if Location.compare l1 l2 >= 0 then l1 else l2

(** Lifts a binary function over two polymorphic [loc] values *)
(* let liftF2 ~(f : 'a -> 'b -> 'c) (alpha : 'a Location.loc)
  (beta : 'b Location.loc) : 'c Location.loc =
  let new_loc = max_loc alpha.loc beta.loc in
  let a, b = (alpha.txt, beta.txt) in
  { txt = f a b; loc = alpha.loc } *)

(** Maps a function component-wise over a pair *)
let map2 ~f (a1, a2) = (f a1, f a2)

(** Converts a triple to a pair *)
let tuple4_to_pair (a, b, _, _) = (a, b)

(** Checks if a list is empty
    - Backwards-compatible version of [List.is_empty], 
    which is only available in OCaml 5.1 and newer *)
let list_is_empty (lst : 'a list) : bool =
  match lst with
  | [] -> true
  | _ -> false

(** Takes the disjunction of a Boolean list
    - The empty list corresponds to false
    - Reimplementation of the [or] function in 
      Haskell's [GHC.Prelude] *)
let rec list_or (xs : bool list) : bool =
  match xs with
  | [] -> false
  | x :: xs -> x || list_or xs

(** Retrieves all elements of a list except the last one *)
let rec remove_last (lst : 'a list) : 'a list =
  match lst with
  | [] | [ _ ] -> []
  | x :: xs -> x :: remove_last xs

(** Returns the final element of a list (if one exists) 
    - Raises an exception if the list is empty *)
let rec get_last (lst : 'a list) : 'a =
  match lst with
  | [] -> failwith "List is empty"
  | [ x ] -> x
  | x :: xs -> get_last xs

(** Swaps the keys & values of an association list.
    - Note: bijectivity is not guaranteed since keys may appear more than once
    in the input association list.
    - Adapted from Jane street's [Base.List.Assoc.inverse] function *)
let invert_assoc_list (lst : ('a * 'b) list) : ('b * 'a) list =
  List.map ~f:(fun (x, y) -> (y, x)) lst

(** [merge_list_with_assoc_list xs yzs ~eq] takes [xs : 'a list] 
    and an association list [yzs : ('b * 'c) list], and creates a 
    new association list of type [('a * 'c) list], using the function [eq] 
    to equate values of type ['a] and ['b] together
    - Raises an exception if there does not exist any element in [xs]
      that [eq] deems to be equal to a key in [yzs] *)
let merge_list_with_assoc_list (xs : 'a list) (yzs : ('b * 'c) list)
  ~(eq : 'a -> 'b -> bool) : ('a * 'c) list =
  List.map yzs ~f:(fun (y, z) ->
      match List.find_opt ~f:(fun x -> eq x y) xs with
      | Some x' -> (x', z)
      | None ->
        failwith "failed to match an element of ['a] with an element of ['b]")

(** Returns true the abstract type declaration in a [signature] 
    is parameterized (e.g. ['a t]), else returns [false] *)
let rec is_abs_ty_parameterized (sig_items : signature) : bool =
  List.fold_left
    ~f:(fun acc { psig_desc; _ } ->
      match psig_desc with
      | Psig_type (_rec_flag, ty_decls) -> (
        match ty_decls with
        | [] -> acc
        | _ ->
          list_or
          @@ List.map
               ~f:(fun { ptype_name; ptype_params; _ } ->
                 String.equal ptype_name.txt "t"
                 && not (list_is_empty ptype_params))
               ty_decls)
      | _ -> acc)
    ~init:false sig_items
