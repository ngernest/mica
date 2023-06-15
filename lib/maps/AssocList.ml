open Base_quickcheck

(** Definition of the [assoc_list] opaque type *)
type assoc_list = (Base.int * Base.string) Base.List.t
  [@@deriving sexp, compare]

(** QuickCheck generator for the [assoc_list] type *)  
let gen_assoc_list : assoc_list Generator.t = 
  let open Base in 
  let open Latin in 
  let open G.Let_syntax in 
  let%bind xs = G.(list_non_empty small_positive_or_zero_int) in 
  let keys = List.dedup_and_sort xs ~compare:compare_int in 
  let length = List.length keys in 
  let%bind values = G.list_with_length ~length genLatin in 
  G.return @@ List.zip_exn keys values   
