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
             | Bool ->
                 let gen_is_empty =
                   let g__014_ = with_size ~size:(k / 2) (gen_expr IntT) in
                   g__014_ >>| (fun e__015_ -> Is_empty e__015_)
                 and gen_mem =
                   let g__016_ = quickcheck_generator_int
                   and g__017_ = with_size ~size:(k / 2) (gen_expr IntT) in
                   (tuple2 g__016_ g__017_) >>|
                     (fun (e__018_, e__019_) -> Mem (e__018_, e__019_))
                 and gen_invariant =
                   let g__020_ = with_size ~size:(k / 2) (gen_expr IntT) in
                   g__020_ >>| (fun e__021_ -> Invariant e__021_) in
                 of_list []
             | Int ->
                 let gen_size =
                   let g__022_ = with_size ~size:(k / 2) (gen_expr IntT) in
                   g__022_ >>| (fun e__023_ -> Size e__023_) in
                 of_list []
             | IntT ->
                 let gen_empty =  in return Empty
                 and gen_add =
                   let g__024_ = quickcheck_generator_int
                   and g__025_ = with_size ~size:(k / 2) (gen_expr IntT) in
                   (tuple2 g__024_ g__025_) >>|
                     (fun (e__026_, e__027_) -> Add (e__026_, e__027_))
                 and gen_rem =
                   let g__028_ = quickcheck_generator_int
                   and g__029_ = with_size ~size:(k / 2) (gen_expr IntT) in
                   (tuple2 g__028_ g__029_) >>|
                     (fun (e__030_, e__031_) -> Rem (e__030_, e__031_))
                 and gen_union =
                   let g__032_ = with_size ~size:(k / 2) (gen_expr IntT)
                   and g__033_ = with_size ~size:(k / 2) (gen_expr IntT) in
                   (tuple2 g__032_ g__033_) >>|
                     (fun (e__034_, e__035_) -> Union (e__034_, e__035_))
                 and gen_intersect =
                   let g__036_ = with_size ~size:(k / 2) (gen_expr IntT)
                   and g__037_ = with_size ~size:(k / 2) (gen_expr IntT) in
                   (tuple2 g__036_ g__037_) >>|
                     (fun (e__038_, e__039_) -> Intersect (e__038_, e__039_)) in
                 of_list [])
let _ = gen_expr
[@@@end]
