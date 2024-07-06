module type SetInterface = sig
  type 'a t

  val empty : 'a t
  val is_empty : 'a t -> bool
  val mem : 'a -> 'a t -> bool
  val add : 'a -> 'a t -> 'a t
  val rem : 'a -> 'a t -> 'a t
  val size : 'a t -> int
  val union : 'a t -> 'a t -> 'a t
  val intersect : 'a t -> 'a t -> 'a t
  val invariant : 'a t -> bool
end
[@@deriving_inline mica_types]

type expr =
  | Empty
  | Is_empty of expr
  | Mem of int * expr
  | Add of int * expr
  | Rem of int * expr
  | Size of expr
  | Union of expr * expr
  | Intersect of expr * expr
  | Invariant of expr

type ty = Bool | Int | IntT

let rec gen_expr ty =
  let open Core in
  let open Quickcheck.Generator in
  let open Let_syntax in
  size >>= fun k ->
  match ty with
  | IntT -> of_list []
  | Bool ->
    of_list
      [ (let gen_is_empty =
           let g__014_ = with_size ~size:(k / 2) (gen_expr T) in
           gen_is_empty in
         gen_is_empty);
        (let gen_mem =
           let g__015_ = quickcheck_generator_int
           and g__016_ = with_size ~size:(k / 2) (gen_expr T) in
           gen_mem in
         gen_mem)
      ]
  | IntT ->
    of_list
      [ (let gen_add =
           let g__017_ = quickcheck_generator_int
           and g__018_ = with_size ~size:(k / 2) (gen_expr T) in
           gen_add in
         gen_add);
        (let gen_rem =
           let g__019_ = quickcheck_generator_int
           and g__020_ = with_size ~size:(k / 2) (gen_expr T) in
           gen_rem in
         gen_rem)
      ]
  | Int ->
    of_list
      [ (let gen_size =
           let g__021_ = with_size ~size:(k / 2) (gen_expr T) in
           gen_size in
         gen_size)
      ]
  | IntT ->
    of_list
      [ (let gen_union =
           let g__022_ = with_size ~size:(k / 2) (gen_expr T)
           and g__023_ = with_size ~size:(k / 2) (gen_expr T) in
           gen_union in
         gen_union);
        (let gen_intersect =
           let g__024_ = with_size ~size:(k / 2) (gen_expr T)
           and g__025_ = with_size ~size:(k / 2) (gen_expr T) in
           gen_intersect in
         gen_intersect)
      ]
  | Bool ->
    of_list
      [ (let gen_invariant =
           let g__026_ = with_size ~size:(k / 2) (gen_expr T) in
           gen_invariant in
         gen_invariant)
      ]

let _ = gen_expr

[@@@end]
