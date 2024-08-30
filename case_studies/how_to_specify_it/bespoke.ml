open All_bsts
open Base
open Base_quickcheck

(** A correct BST insertion function *)
let rec insert_correct k v t =
  match t with
  | Leaf -> branch Leaf k v Leaf
  | Branch (l, k', v', r) when k < k' -> branch (insert_correct k v l) k' v' r
  | Branch (l, k', v', r) when k > k' -> branch l k' v' (insert_correct k v r)
  | Branch (l, k', v', r) -> branch l k' v r

(** Generates a key in the range [[0, size]] *)
let gen_int : int Generator.t =
  let open Generator in
  size >>= fun k -> int_inclusive 0 k

(** Bespoke generator for BSTs, uses the [insert] function *)
let quickcheck_generator_t : (int, int) Bst.t Generator.t =
  let open Generator in
  list (both gen_int gen_int) >>| fun xs ->
  List.fold_left ~init:Leaf ~f:(fun acc (k, v) -> insert_correct k v acc) xs

(* TODO: hand-write a bespoke test suite that replicates Mica's functionality? *)  