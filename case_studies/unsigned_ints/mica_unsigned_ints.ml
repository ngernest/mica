open Unsigned_int_impls

(******************************************************************************)
(* Code automatically derived by Mica below *)

module Mica = struct
  include struct
    type expr =
      | Add of expr * expr
      | Sub of expr * expr
      | Mul of expr * expr
      | Max_int
      | Logand of expr * expr
      | Logor of expr * expr
      | Logxor of expr * expr
      | Shift_left of expr * int
      | Shift_right of expr * int
      | Of_int of int
      | To_int of expr
      | To_string of expr
      | Zero
      | One
      | Lognot of expr
      | Succ of expr
      | Pred of expr
      | Equal_t of expr * expr
      | Compare of expr * expr
      | Of_int64 of int64
      | To_int64 of expr
    [@@deriving show { with_path = false }]

    type ty = Bool | Int | Int64 | String | T
    [@@deriving show { with_path = false }]

    let rec gen_expr ty =
      let open Core in
      let open Quickcheck.Generator in
      let open Let_syntax in
      size >>= fun k ->
      match (ty, k) with
      | Bool, _ ->
        let gen_equal =
          let g__001_ = with_size ~size:(k / 2) (gen_expr T)
          and g__002_ = with_size ~size:(k / 2) (gen_expr T) in
          tuple2 g__001_ g__002_ >>| fun (e__003_, e__004_) ->
          Equal_t (e__003_, e__004_) in
        union [ gen_equal ]
      | Int64, _ ->
        let gen_to_int64 =
          let g = with_size ~size:(k / 2) (gen_expr T) in
          g >>| fun e -> To_int64 e in
        union [ gen_to_int64 ]
      | Int, _ ->
        let gen_to_int =
          let g__005_ = with_size ~size:(k / 2) (gen_expr T) in
          g__005_ >>| fun e__006_ -> To_int e__006_
        and gen_compare =
          let g1 = with_size ~size:(k / 2) (gen_expr T)
          and g2 = with_size ~size:(k / 2) (gen_expr T) in
          tuple2 g1 g2 >>| fun (e1, e2) -> Compare (e1, e2) in
        union [ gen_to_int; gen_compare ]
      | String, _ ->
        let gen_to_string =
          let g__007_ = with_size ~size:(k / 2) (gen_expr T) in
          g__007_ >>| fun e__008_ -> To_string e__008_ in
        union [ gen_to_string ]
      | T, 0 -> union [ return One; return Zero; return Max_int ]
      | T, _ ->
        let gen_add =
          let g__009_ = with_size ~size:(k / 2) (gen_expr T)
          and g__010_ = with_size ~size:(k / 2) (gen_expr T) in
          tuple2 g__009_ g__010_ >>| fun (e__011_, e__012_) ->
          Add (e__011_, e__012_)
        and gen_sub =
          let g__013_ = with_size ~size:(k / 2) (gen_expr T)
          and g__014_ = with_size ~size:(k / 2) (gen_expr T) in
          tuple2 g__013_ g__014_ >>| fun (e__015_, e__016_) ->
          Sub (e__015_, e__016_)
        and gen_mul =
          let g__017_ = with_size ~size:(k / 2) (gen_expr T)
          and g__018_ = with_size ~size:(k / 2) (gen_expr T) in
          tuple2 g__017_ g__018_ >>| fun (e__019_, e__020_) ->
          Mul (e__019_, e__020_)
        and gen_logand =
          let g__021_ = with_size ~size:(k / 2) (gen_expr T)
          and g__022_ = with_size ~size:(k / 2) (gen_expr T) in
          tuple2 g__021_ g__022_ >>| fun (e__023_, e__024_) ->
          Logand (e__023_, e__024_)
        and gen_logor =
          let g__025_ = with_size ~size:(k / 2) (gen_expr T)
          and g__026_ = with_size ~size:(k / 2) (gen_expr T) in
          tuple2 g__025_ g__026_ >>| fun (e__027_, e__028_) ->
          Logor (e__027_, e__028_)
        and gen_logxor =
          let g__029_ = with_size ~size:(k / 2) (gen_expr T)
          and g__030_ = with_size ~size:(k / 2) (gen_expr T) in
          tuple2 g__029_ g__030_ >>| fun (e__031_, e__032_) ->
          Logxor (e__031_, e__032_)
        and gen_shift_left =
          let g__033_ = with_size ~size:(k / 2) (gen_expr T)
          and g__034_ = quickcheck_generator_int in
          tuple2 g__033_ g__034_ >>| fun (e__035_, e__036_) ->
          Shift_left (e__035_, e__036_)
        and gen_shift_right =
          let g__037_ = with_size ~size:(k / 2) (gen_expr T)
          and g__038_ = quickcheck_generator_int in
          tuple2 g__037_ g__038_ >>| fun (e__039_, e__040_) ->
          Shift_right (e__039_, e__040_)
        and gen_of_int =
          let g__041_ = quickcheck_generator_int in
          g__041_ >>| fun e__042_ -> Of_int e__042_
        and gen_lognot =
          let g__045_ = with_size ~size:(k / 2) (gen_expr T) in
          g__045_ >>| fun e__046_ -> Lognot e__046_
        and gen_succ =
          let g__047_ = with_size ~size:(k / 2) (gen_expr T) in
          g__047_ >>| fun e__048_ -> Succ e__048_
        and gen_pred =
          let g__049_ = with_size ~size:(k / 2) (gen_expr T) in
          g__049_ >>| fun e__050_ -> Pred e__050_
        and gen_of_int64 =
          let g = quickcheck_generator_int64 in
          g >>| fun e -> Of_int64 e in
        union
          [ gen_add;
            gen_sub;
            gen_mul;
            gen_logand;
            gen_logor;
            gen_logxor;
            gen_shift_left;
            gen_shift_right;
            gen_of_int;
            gen_lognot;
            gen_succ;
            gen_pred;
            gen_of_int64
          ]
  end

  module Interpret (M : S) = struct
    open M

    type value =
      | ValBool of bool
      | ValInt of int
      | ValString of string
      | ValT of t
      | ValInt64 of int64

    let rec interp e =
      match e with
      | Add (expr__051_, expr__052_) -> (
        match (interp expr__051_, interp expr__052_) with
        | ValT expr__051_', ValT expr__052_' ->
          ValT (M.add expr__051_' expr__052_')
        | _ -> failwith "impossible: n-ary constructor")
      | Sub (expr__053_, expr__054_) -> (
        match (interp expr__053_, interp expr__054_) with
        | ValT expr__053_', ValT expr__054_' ->
          ValT (M.sub expr__053_' expr__054_')
        | _ -> failwith "impossible: n-ary constructor")
      | Mul (expr__055_, expr__056_) -> (
        match (interp expr__055_, interp expr__056_) with
        | ValT expr__055_', ValT expr__056_' ->
          ValT (M.mul expr__055_' expr__056_')
        | _ -> failwith "impossible: n-ary constructor")
      | Max_int -> ValT M.max_int
      | Logand (expr__057_, expr__058_) -> (
        match (interp expr__057_, interp expr__058_) with
        | ValT expr__057_', ValT expr__058_' ->
          ValT (M.logand expr__057_' expr__058_')
        | _ -> failwith "impossible: n-ary constructor")
      | Logor (expr__059_, expr__060_) -> (
        match (interp expr__059_, interp expr__060_) with
        | ValT expr__059_', ValT expr__060_' ->
          ValT (M.logor expr__059_' expr__060_')
        | _ -> failwith "impossible: n-ary constructor")
      | Logxor (expr__061_, expr__062_) -> (
        match (interp expr__061_, interp expr__062_) with
        | ValT expr__061_', ValT expr__062_' ->
          ValT (M.logxor expr__061_' expr__062_')
        | _ -> failwith "impossible: n-ary constructor")
      | Shift_left (expr__063_, int__064_) -> (
        match interp expr__063_ with
        | ValT expr__063_' -> ValT (M.shift_left expr__063_' int__064_)
        | _ -> failwith "impossible: n-ary constructor")
      | Shift_right (expr__065_, int__066_) -> (
        match interp expr__065_ with
        | ValT expr__065_' -> ValT (M.shift_right expr__065_' int__066_)
        | _ -> failwith "impossible: n-ary constructor")
      | Of_int int__067_ -> ValT (M.of_int int__067_)
      | To_int expr__068_ -> (
        match interp expr__068_ with
        | ValT expr__068_' -> ValInt (M.to_int expr__068_')
        | _ -> failwith "impossible: unary constructor")
      | To_string expr__070_ -> (
        match interp expr__070_ with
        | ValT expr__070_' -> ValString (M.to_string expr__070_')
        | _ -> failwith "impossible: unary constructor")
      | Zero -> ValT M.zero
      | One -> ValT M.one
      | Lognot expr__071_ -> (
        match interp expr__071_ with
        | ValT expr__071_' -> ValT (M.lognot expr__071_')
        | _ -> failwith "impossible: unary constructor")
      | Succ expr__072_ -> (
        match interp expr__072_ with
        | ValT expr__072_' -> ValT (M.succ expr__072_')
        | _ -> failwith "impossible: unary constructor")
      | Pred expr__073_ -> (
        match interp expr__073_ with
        | ValT expr__073_' -> ValT (M.pred expr__073_')
        | _ -> failwith "impossible: unary constructor")
      | Equal_t (expr__074_, expr__075_) -> (
        match (interp expr__074_, interp expr__075_) with
        | ValT expr__074_', ValT expr__075_' ->
          ValBool (M.equal expr__074_' expr__075_')
        | _ -> failwith "impossible: n-ary constructor")
      | Compare (e1, e2) -> (
        match (interp e1, interp e2) with
        | ValT e1', ValT e2' -> ValInt (M.compare e1' e2')
        | _ -> failwith "impossible: n-ary constructor")
      | Of_int64 i -> ValT (M.of_int64 i)
      | To_int64 e -> (
        match interp e with
        | ValT e' -> ValInt64 (M.to_int64 e')
        | _ -> failwith "impossible")
  end

  module TestHarness (M1 : S) (M2 : S) = struct
    module I1 = Interpret (M1)
    module I2 = Interpret (M2)
    open Core

    let test_bool () : unit =
      Quickcheck.test (gen_expr Bool) ~f:(fun e ->
          match (I1.interp e, I2.interp e) with
          | ValBool b1, ValBool b2 -> [%test_eq: bool] b1 b2
          | _ -> failwith "failed bool")

    let test_int () : unit =
      Quickcheck.test (gen_expr Int) ~f:(fun e ->
          match (I1.interp e, I2.interp e) with
          | ValInt i1, ValInt i2 -> [%test_eq: int] i1 i2
          | _ -> failwith "failed int")

    let test_int64 () : unit =
      Quickcheck.test (gen_expr Int64) ~f:(fun e ->
          match (I1.interp e, I2.interp e) with
          | ValInt64 i1, ValInt64 i2 -> [%test_eq: int64] i1 i2
          | _ -> failwith "failed int")

    let test_string () : unit =
      Quickcheck.test (gen_expr String) ~f:(fun e ->
          match (I1.interp e, I2.interp e) with
          | ValString s1, ValString s2 -> [%test_eq: string] s1 s2
          | _ -> failwith "failed int")

    let run_tests () : unit =
      test_bool ();
      test_int ();
      test_int64 ();
      test_string ();
      printf "Mica: OK, passed %d observational equivalence tests.\n" 40000
  end
end

(******************************************************************************)
(* Using Mica to check observational equivalence of two implementations of
   8/16/32/64-bit unsigned integer arithmetic below: *)

let () =
  let module T8 = Mica.TestHarness (U8_1) (U8_2) in
  let module T16 = Mica.TestHarness (U16_1) (U16_2) in
  let module T32 = Mica.TestHarness (U32_1) (U32_2) in
  let module T64 = Mica.TestHarness (U64_1) (U64_2) in
  T8.run_tests ();
  Stdio.printf "passed u8 tests\n";
  T16.run_tests ();
  Stdio.printf "passed u16 tests\n";
  T32.run_tests ();
  Stdio.printf "passed u32 tests\n";
  T64.run_tests ();
  Stdio.printf "passed u64 tests\n"
