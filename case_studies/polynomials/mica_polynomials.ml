open Polynomial_signature
open Poly1
open Poly2

(******************************************************************************)
(** The following is very similar to the code that Mica would generate
   automatically. The only manual modifications are: 
   - Variable renaming
   - The use of [Base_quickcheck]'s [small_positive_or_zero_int] generator
   (to generate non-negative integers when raising polynomials to a power)
   - certain [let open] statements to prevent namespace clashes *)

module Mica = struct
  include struct
    type expr =
      | Zero
      | One
      | Power of int * int
      | Monomial of int * int
      | Add of expr * expr
      | Mult of expr * expr
      | Create of (int * int) list
      | Eval of expr * int
      | Equal of expr * expr
    [@@deriving show { with_path = false }]

    type ty = Bool | Int | T [@@deriving show { with_path = false }]

    let rec gen_expr ty =
      let open Core.Quickcheck.Generator in
      let open Let_syntax in
      size >>= fun k ->
      match (ty, k) with
      | Bool, _ ->
        let gen_equal =
          let g__001_ = with_size ~size:(k / 2) (gen_expr T)
          and g__002_ = with_size ~size:(k / 2) (gen_expr T) in
          tuple2 g__001_ g__002_ >>| fun (e__003_, e__004_) ->
          Equal (e__003_, e__004_) in
        union [ gen_equal ]
      | Int, _ ->
        let gen_power =
          let g__005_ = Base_quickcheck.Generator.small_positive_or_zero_int
          and g__006_ = Base_quickcheck.Generator.small_positive_or_zero_int in
          tuple2 g__005_ g__006_ >>| fun (e__007_, e__008_) ->
          Power (e__007_, e__008_)
        and gen_eval =
          let g__009_ = with_size ~size:(k / 2) (gen_expr T)
          and g__010_ = Base_quickcheck.Generator.small_positive_or_zero_int in
          tuple2 g__009_ g__010_ >>| fun (e__011_, e__012_) ->
          Eval (e__011_, e__012_) in
        union [ gen_power; gen_eval ]
      | T, 0 ->
        let gen_zero = return Zero
        and gen_one = return One
        and gen_monomial =
          let g__013_ = Base_quickcheck.Generator.small_positive_or_zero_int
          and g__014_ = Base_quickcheck.Generator.small_positive_or_zero_int in
          tuple2 g__013_ g__014_ >>| fun (e__015_, e__016_) ->
          Monomial (e__015_, e__016_)
        and gen_create =
          let g__017_ =
            Core.quickcheck_generator_list
              (tuple2 Base_quickcheck.Generator.small_positive_or_zero_int
                 Base_quickcheck.Generator.small_positive_or_zero_int) in
          g__017_ >>| fun e__018_ -> Create e__018_ in
        union [ gen_zero; gen_one; gen_monomial; gen_create ]
      | T, _ ->
        let gen_add =
          let g__019_ = with_size ~size:(k / 2) (gen_expr T)
          and g__020_ = with_size ~size:(k / 2) (gen_expr T) in
          tuple2 g__019_ g__020_ >>| fun (e__021_, e__022_) ->
          Add (e__021_, e__022_)
        and gen_mult =
          let g__023_ = with_size ~size:(k / 2) (gen_expr T)
          and g__024_ = with_size ~size:(k / 2) (gen_expr T) in
          tuple2 g__023_ g__024_ >>| fun (e__025_, e__026_) ->
          Mult (e__025_, e__026_) in
        union [ gen_add; gen_mult ]

    let _ = gen_expr
  end

  module Interpret (M : S) = struct
    open M

    type value = ValBool of bool | ValInt of int | ValT of t

    let rec interp e =
      match e with
      | Zero -> ValT M.zero
      | One -> ValT M.one
      | Power (int__027_, int__028_) -> ValInt (M.power int__027_ int__028_)
      | Monomial (int__029_, int__030_) -> ValT (M.monomial int__029_ int__030_)
      | Add (expr__031_, expr__032_) -> (
        match (interp expr__031_, interp expr__032_) with
        | ValT expr__031_', ValT expr__032_' ->
          ValT (M.add expr__031_' expr__032_')
        | _ -> failwith "impossible: n-ary constructor")
      | Mult (expr__033_, expr__034_) -> (
        match (interp expr__033_, interp expr__034_) with
        | ValT expr__033_', ValT expr__034_' ->
          ValT (M.mult expr__033_' expr__034_')
        | _ -> failwith "impossible: n-ary constructor")
      | Create intIntProductList__035_ ->
        ValT (M.create intIntProductList__035_)
      | Eval (expr__036_, int__037_) -> (
        match interp expr__036_ with
        | ValT expr__036_' -> ValInt (M.eval expr__036_' int__037_)
        | _ -> failwith "impossible: n-ary constructor")
      | Equal (expr__038_, expr__039_) -> (
        match (interp expr__038_, interp expr__039_) with
        | ValT expr__038_', ValT expr__039_' ->
          ValBool (M.equal expr__038_' expr__039_')
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
              | ValBool bool__041_, ValBool bool__040_ ->
                [%test_eq: bool] bool__041_ bool__040_
              | _ -> failwith "impossible")

        let _ = test_bool

        let test_int () =
          Quickcheck.test (gen_expr Int) ~f:(fun e ->
              match (I1.interp e, I2.interp e) with
              | ValInt int__043_, ValInt int__042_ ->
                [%test_eq: int] int__043_ int__042_
              | _ -> failwith "impossible")

        let _ = test_int

        let run_tests () =
          test_bool ();
          test_int ();
          printf "Mica: OK, passed %d observational equivalence tests.\n" 20000

        let _ = run_tests
      end
    end
  end
end

(******************************************************************************)
(* Using Mica to check observational equivalence of two implementations of
   polynomials below: *)

module T = Mica.TestHarness (Poly1) (Poly2)

let () = T.run_tests ()
