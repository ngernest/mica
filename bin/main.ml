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
end

include struct
  [@@@ocaml.warning "-60"]

  module Mica = struct
    include struct
      type expr =
        | Empty
        | Is_empty of expr
        | Mem of int * expr
        | Add of int * expr
        | Rem of int * expr
        | Size of expr
        | Union of expr * expr
        | Intersect of expr * expr
      [@@deriving show { with_path = false }]

      type ty = Bool | Int | IntT [@@deriving show { with_path = false }]

      let rec gen_expr ty =
        let open Core in
        let open Quickcheck.Generator in
        let open Let_syntax in
        size >>= fun k ->
        match ty with
        | Bool ->
          let gen_is_empty =
            let g__001_ = with_size ~size:(k / 2) (gen_expr IntT) in
            g__001_ >>| fun e__002_ -> Is_empty e__002_
          and gen_mem =
            let g__003_ = quickcheck_generator_int
            and g__004_ = with_size ~size:(k / 2) (gen_expr IntT) in
            tuple2 g__003_ g__004_ >>| fun (e__005_, e__006_) ->
            Mem (e__005_, e__006_) in
          union [ gen_is_empty; gen_mem ]
        | Int ->
          let gen_size =
            let g__007_ = with_size ~size:(k / 2) (gen_expr IntT) in
            g__007_ >>| fun e__008_ -> Size e__008_ in
          union [ gen_size ]
        | IntT ->
          let gen_empty = return Empty
          and gen_add =
            let g__009_ = quickcheck_generator_int
            and g__010_ = with_size ~size:(k / 2) (gen_expr IntT) in
            tuple2 g__009_ g__010_ >>| fun (e__011_, e__012_) ->
            Add (e__011_, e__012_)
          and gen_rem =
            let g__013_ = quickcheck_generator_int
            and g__014_ = with_size ~size:(k / 2) (gen_expr IntT) in
            tuple2 g__013_ g__014_ >>| fun (e__015_, e__016_) ->
            Rem (e__015_, e__016_)
          and gen_union =
            let g__017_ = with_size ~size:(k / 2) (gen_expr IntT)
            and g__018_ = with_size ~size:(k / 2) (gen_expr IntT) in
            tuple2 g__017_ g__018_ >>| fun (e__019_, e__020_) ->
            Union (e__019_, e__020_)
          and gen_intersect =
            let g__021_ = with_size ~size:(k / 2) (gen_expr IntT)
            and g__022_ = with_size ~size:(k / 2) (gen_expr IntT) in
            tuple2 g__021_ g__022_ >>| fun (e__023_, e__024_) ->
            Intersect (e__023_, e__024_) in
          union [ gen_empty; gen_add; gen_rem; gen_union; gen_intersect ]

      let _ = gen_expr
    end

    module Interpret (M : S) = struct
      open M

      type value = ValBool of bool | ValInt of int | ValIntT of int t

      let rec interp e =
        match e with
        | Empty -> ValIntT M.empty
        | Is_empty expr__025_ -> (
          match interp expr__025_ with
          | ValIntT expr__025_' -> ValBool (M.is_empty expr__025_')
          | _ -> failwith "impossible: unary constructor")
        | Mem (int__026_, expr__027_) -> (
          match interp expr__027_ with
          | ValIntT expr__027_' -> ValBool (M.mem int__026_ expr__027_')
          | _ -> failwith "impossible: n-ary constructor")
        | Add (int__028_, expr__029_) -> (
          match interp expr__029_ with
          | ValIntT expr__029_' -> ValIntT (M.add int__028_ expr__029_')
          | _ -> failwith "impossible: n-ary constructor")
        | Rem (int__030_, expr__031_) -> (
          match interp expr__031_ with
          | ValIntT expr__031_' -> ValIntT (M.rem int__030_ expr__031_')
          | _ -> failwith "impossible: n-ary constructor")
        | Size expr__032_ -> (
          match interp expr__032_ with
          | ValIntT expr__032_' -> ValInt (M.size expr__032_')
          | _ -> failwith "impossible: unary constructor")
        | Union (expr__033_, expr__034_) -> (
          match (interp expr__033_, interp expr__034_) with
          | ValIntT expr__033_', ValIntT expr__034_' ->
            ValIntT (M.union expr__033_' expr__034_')
          | _ -> failwith "impossible: n-ary constructor")
        | Intersect (expr__035_, expr__036_) -> (
          match (interp expr__035_, interp expr__036_) with
          | ValIntT expr__035_', ValIntT expr__036_' ->
            ValIntT (M.intersect expr__035_' expr__036_')
          | _ -> failwith "impossible: n-ary constructor")

      let _ = interp
    end

    include struct
      module TestHarness (M1 : S) (M2 : S) = struct
        module I1 = Interpret (M1)
        module I2 = Interpret (M2)
        open Core

        include struct
          let test_bool () =
            Quickcheck.test (gen_expr Bool) ~f:(fun e ->
                match (I1.interp e, I2.interp e) with
                | ValBool bool__038_, ValBool bool__037_ ->
                  [%test_eq: bool] bool__038_ bool__037_
                | _ -> failwith "impossible")

          let _ = test_bool

          let test_int () =
            Quickcheck.test (gen_expr Int) ~f:(fun e ->
                match (I1.interp e, I2.interp e) with
                | ValInt int__040_, ValInt int__039_ ->
                  [%test_eq: int] int__040_ int__039_
                | _ -> failwith "impossible")

          let _ = test_int
        end
      end
    end
  end
end [@@ocaml.doc "@inline"]
