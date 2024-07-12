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
             | IntT -> [(let gen_empty =  in return Empty in gen_empty)]
             | Bool ->
                 [(let gen_is_empty =
                     let g__014_ = with_size ~size:(k / 2) (gen_expr T) in
                     g__014_ >>| (fun e__015_ -> Is_empty e__015_) in
                   gen_is_empty);
                 (let gen_mem =
                    let g__016_ = quickcheck_generator_int
                    and g__017_ = with_size ~size:(k / 2) (gen_expr T) in
                    (tuple2 g__016_ g__017_) >>|
                      (fun (e__018_, e__019_) -> Mem (e__018_, e__019_)) in
                  gen_mem)]
             | IntT ->
                 [(let gen_add =
                     let g__020_ = quickcheck_generator_int
                     and g__021_ = with_size ~size:(k / 2) (gen_expr T) in
                     (tuple2 g__020_ g__021_) >>|
                       (fun (e__022_, e__023_) -> Add (e__022_, e__023_)) in
                   gen_add);
                 (let gen_rem =
                    let g__024_ = quickcheck_generator_int
                    and g__025_ = with_size ~size:(k / 2) (gen_expr T) in
                    (tuple2 g__024_ g__025_) >>|
                      (fun (e__026_, e__027_) -> Rem (e__026_, e__027_)) in
                  gen_rem)]
             | Int ->
                 [(let gen_size =
                     let g__028_ = with_size ~size:(k / 2) (gen_expr T) in
                     g__028_ >>| (fun e__029_ -> Size e__029_) in
                   gen_size)]
             | IntT ->
                 [(let gen_union =
                     let g__030_ = with_size ~size:(k / 2) (gen_expr T)
                     and g__031_ = with_size ~size:(k / 2) (gen_expr T) in
                     (tuple2 g__030_ g__031_) >>|
                       (fun (e__032_, e__033_) -> Union (e__032_, e__033_)) in
                   gen_union);
                 (let gen_intersect =
                    let g__034_ = with_size ~size:(k / 2) (gen_expr T)
                    and g__035_ = with_size ~size:(k / 2) (gen_expr T) in
                    (tuple2 g__034_ g__035_) >>|
                      (fun (e__036_, e__037_) -> Intersect (e__036_, e__037_)) in
                  gen_intersect)]
             | Bool ->
                 [(let gen_invariant =
                     let g__038_ = with_size ~size:(k / 2) (gen_expr T) in
                     g__038_ >>| (fun e__039_ -> Invariant e__039_) in
                   gen_invariant)])
let _ = gen_expr
[@@@end]
