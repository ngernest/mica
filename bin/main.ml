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
type ty =
  | Bool
  | Int
  | IntT
let rec gen_expr ty =
  let open Core in
    let open Quickcheck.Generator in
      let open Let_syntax in
        size >>=
          (fun k ->
             match ty with
             | IntT -> of_list [[]]
             | Bool ->
                 of_list
                   [(let __0 () = with_size ~size:(k / 2) (gen_expr T) in
                     [__0 ()]);
                   (let __1 () = with_size ~size:(k / 2) (gen_expr T)
                    and __0 = quickcheck_generator_int in [__0; __1 ()])]
             | IntT ->
                 of_list
                   [(let __1 () = with_size ~size:(k / 2) (gen_expr T)
                     and __0 = quickcheck_generator_int in [__0; __1 ()]);
                   (let __1 () = with_size ~size:(k / 2) (gen_expr T)
                    and __0 = quickcheck_generator_int in [__0; __1 ()])]
             | Int ->
                 of_list
                   [(let __0 () = with_size ~size:(k / 2) (gen_expr T) in
                     [__0 ()])]
             | IntT ->
                 of_list
                   [(let __1 () = with_size ~size:(k / 2) (gen_expr T)
                     and __0 () = with_size ~size:(k / 2) (gen_expr T) in
                     [__0 (); __1 ()]);
                   (let __1 () = with_size ~size:(k / 2) (gen_expr T)
                    and __0 () = with_size ~size:(k / 2) (gen_expr T) in
                    [__0 (); __1 ()])]
             | Bool ->
                 of_list
                   [(let __0 () = with_size ~size:(k / 2) (gen_expr T) in
                     [__0 ()])])
let _ = gen_expr
