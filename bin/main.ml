module type S = sig
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

include struct
  [@@@ocaml.warning "-60"]

  module Interpret (M : S) = struct
    open M

    type value = ValBool of bool | ValInt of int | ValIntT of int t

    let rec interp e =
      match e with
      | Empty -> ValIntT M.empty
      | Is_empty expr__001_ -> (
        match interp expr__001_ with
        | ValIntT expr__001_' -> ValBool (M.is_empty expr__001_')
        | _ -> failwith "impossible: unary constructor")
      | Mem (int__002_, expr__003_) -> (
        match interp expr__003_ with
        | ValIntT expr__003_' -> ValBool (M.mem int__002_ expr__003_')
        | _ -> failwith "impossible: n-ary constructor")
      | Add (int__004_, expr__005_) -> (
        match interp expr__005_ with
        | ValIntT expr__005_' -> ValIntT (M.add int__004_ expr__005_')
        | _ -> failwith "impossible: n-ary constructor")
      | Rem (int__006_, expr__007_) -> (
        match interp expr__007_ with
        | ValIntT expr__007_' -> ValIntT (M.rem int__006_ expr__007_')
        | _ -> failwith "impossible: n-ary constructor")
      | Size expr__008_ -> (
        match interp expr__008_ with
        | ValIntT expr__008_' -> ValInt (M.size expr__008_')
        | _ -> failwith "impossible: unary constructor")
      | Union (expr__009_, expr__010_) -> (
        match (interp expr__009_, interp expr__010_) with
        | ValIntT expr__009_', ValIntT expr__010_' ->
          ValIntT (M.union expr__009_' expr__010_')
        | _ -> failwith "impossible: n-ary constructor")
      | Intersect (expr__011_, expr__012_) -> (
        match (interp expr__011_, interp expr__012_) with
        | ValIntT expr__011_', ValIntT expr__012_' ->
          ValIntT (M.intersect expr__011_' expr__012_')
        | _ -> failwith "impossible: n-ary constructor")
      | Invariant expr__013_ -> (
        match interp expr__013_ with
        | ValIntT expr__013_' -> ValBool (M.invariant expr__013_')
        | _ -> failwith "impossible: unary constructor")

    let _ = interp
  end

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
  [@@deriving show { with_path = false }]

  type ty = Bool | Int | IntT

  let rec gen_expr ty =
    let open Core in
    let open Quickcheck.Generator in
    let open Let_syntax in
    size >>= fun k ->
    match ty with
    | Bool ->
      let gen_is_empty =
        let g__014_ = with_size ~size:(k / 2) (gen_expr IntT) in
        g__014_ >>| fun e__015_ -> Is_empty e__015_
      and gen_mem =
        let g__016_ = quickcheck_generator_int
        and g__017_ = with_size ~size:(k / 2) (gen_expr IntT) in
        tuple2 g__016_ g__017_ >>| fun (e__018_, e__019_) ->
        Mem (e__018_, e__019_)
      and gen_invariant =
        let g__020_ = with_size ~size:(k / 2) (gen_expr IntT) in
        g__020_ >>| fun e__021_ -> Invariant e__021_ in
      union [ gen_is_empty; gen_mem; gen_invariant ]
    | Int ->
      let gen_size =
        let g__022_ = with_size ~size:(k / 2) (gen_expr IntT) in
        g__022_ >>| fun e__023_ -> Size e__023_ in
      union [ gen_size ]
    | IntT ->
      let gen_empty = return Empty
      and gen_add =
        let g__024_ = quickcheck_generator_int
        and g__025_ = with_size ~size:(k / 2) (gen_expr IntT) in
        tuple2 g__024_ g__025_ >>| fun (e__026_, e__027_) ->
        Add (e__026_, e__027_)
      and gen_rem =
        let g__028_ = quickcheck_generator_int
        and g__029_ = with_size ~size:(k / 2) (gen_expr IntT) in
        tuple2 g__028_ g__029_ >>| fun (e__030_, e__031_) ->
        Rem (e__030_, e__031_)
      and gen_union =
        let g__032_ = with_size ~size:(k / 2) (gen_expr IntT)
        and g__033_ = with_size ~size:(k / 2) (gen_expr IntT) in
        tuple2 g__032_ g__033_ >>| fun (e__034_, e__035_) ->
        Union (e__034_, e__035_)
      and gen_intersect =
        let g__036_ = with_size ~size:(k / 2) (gen_expr IntT)
        and g__037_ = with_size ~size:(k / 2) (gen_expr IntT) in
        tuple2 g__036_ g__037_ >>| fun (e__038_, e__039_) ->
        Intersect (e__038_, e__039_) in
      union [ gen_empty; gen_add; gen_rem; gen_union; gen_intersect ]

  let _ = gen_expr
end [@@ocaml.doc "@inline"]
