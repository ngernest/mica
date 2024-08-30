open Base_quickcheck
open Base

(* Suppress "unused-values" compiler warnings *)
[@@@ocaml.warning "-32-34-27"]

(** [T] is a auxiliary module defining an abstract type & its comparison function,
    following the convention in Jane Street's [Base.Compare] module
    - See https://ocaml.janestreet.com/ocaml-core/v0.12/doc/base/Base/Comparable/index.html *)
module T = struct
  (** Definition of the [assoc_list] opaque type *)
  type t = (int * string) list [@@deriving sexp]

  (** Polymorphic lexicographic comparison for pair types ['a * 'b], using 
    the default [compare] functions for types ['a] and ['b]. 
    For two pairs [(a1, b1), (a2, b2)]:
    - If [compare a1 a2 < 0], [lex_compare_pair (a1, _) (a2, _) < 0]
    - If [compare a1 a2 > 0], [lex_compare_pair (a1, _) (a2, _) > 0]
    - If [compare a1 a2 = 0], [lex_compare_pair (a1, b1) (a2, b2) = compare b1 b2]
    - Adapted from https://stackoverflow.com/a/20348513
  *)
  let lex_compare_pair ((a1, b1) : int * string) ((a2, b2) : int * string) : int
      =
    let compare_fst = compare a1 a2 in
    match compare_fst with
    | 0 -> String.compare b1 b2
    | _ -> compare_fst

  (** Comparison function for the [assoc_list] opaque type *)
  let compare (lst1 : t) (lst2 : t) : int =
    let uniq1 = List.dedup_and_sort ~compare:lex_compare_pair lst1 in
    let uniq2 = List.dedup_and_sort ~compare:lex_compare_pair lst2 in
    List.compare lex_compare_pair uniq1 uniq2
end

include T
include Comparable.Make (T)

(** QuickCheck generator for the [assoc_list] type 
    - Note: For any user-defined opaque type [M.t] (where [M] is a module), 
    we require that the QuickCheck generator be called [quickcheck_generator], 
    following the naming conventions of Jane Street's 
    {{: https://github.com/janestreet/base_quickcheck/tree/master/ppx_quickcheck} ppx_quickcheck}
    library. *)
let quickcheck_generator : t Generator.t =
  let open Base in
  let open Latin in
  let open G.Let_syntax in
  let%bind xs = G.(list_non_empty small_positive_or_zero_int) in
  let keys = List.dedup_and_sort xs ~compare:compare_int in
  let length = List.length keys in
  let%bind values = G.list_with_length ~length genLatin in
  G.return @@ List.zip_exn keys values
