open Base
open Base_quickcheck
open Map_signature
open Rbt
open AssocList
open AssocListMap
open Latin

(******************************************************************************)
(** The following is very similar to the code that Mica would generate
   automatically. The only manual modifications are: 
   - Variable renaming 
   - The use of an auxiliary [genLatin] QuickCheck generator to generate 
   Latin words (instead of any arbitrary string using 
   [quickcheck_generator_string]) *)

module Mica = struct
  type expr =
    | Empty
    | Insert of (int * string) * expr
    | Find of int * expr
    | Remove of int * expr
    | From_list of AssocList.t
    | Bindings of expr

  type ty = AssocList | StringOption | T

  module Interpret (M : S) = struct
    type value =
      | ValAssocList of AssocList.t
      | ValStringOption of string option
      | ValT of M.t

    let rec interp (expr : expr) : value =
      match expr with
      | Empty -> ValT M.empty
      | Insert (p1, e2) -> (
        match interp e2 with
        | ValT e' -> ValT (M.insert p1 e')
        | _ -> failwith "impossible")
      | Find (n1, e2) -> (
        match interp e2 with
        | ValT e' -> ValStringOption (M.find n1 e')
        | _ -> failwith "impossible")
      | Remove (n1, e2) -> (
        match interp e2 with
        | ValT e' -> ValT (M.remove n1 e')
        | _ -> failwith "impossible")
      | From_list ps -> ValT (M.from_list ps)
      | Bindings e -> (
        match interp e with
        | ValT e' -> ValAssocList (M.bindings e')
        | _ -> failwith "impossible")
  end

  let rec gen_expr (ty : ty) : expr Generator.t =
    let module G = Generator in
    let open G.Let_syntax in
    let%bind k = G.size in
    match (ty, k) with
    | T, 0 -> return Empty
    | AssocList, _ ->
      let bindings =
        let%bind e = G.with_size ~size:(k / 2) (gen_expr T) in
        G.return @@ Bindings e in
      bindings
    | StringOption, _ ->
      let find =
        let%bind n1 = G.int_inclusive (-10) 10 in
        let%bind e2 = G.with_size ~size:(k / 2) (gen_expr T) in
        G.return @@ Find (n1, e2) in
      find
    | T, _ ->
      let insert =
        let%bind ((n1, s2) as p1) = G.both (G.int_inclusive (-10) 10) genLatin in
        let%bind e2 = G.with_size ~size:(k / 2) (gen_expr T) in
        G.return @@ Insert (p1, e2) in
      let remove =
        let%bind n1 = G.int_inclusive (-10) 10 in
        let%bind e2 = G.with_size ~size:(k / 2) (gen_expr T) in
        G.return @@ Remove (n1, e2) in
      let from_list =
        let%bind ps = [%quickcheck.generator: AssocList.t] in
        G.return @@ From_list ps in
      G.union [ insert; remove; from_list ]

  module TestHarness (M1 : S) (M2 : S) = struct
    module I1 = Interpret (M1)
    module I2 = Interpret (M2)
    open Core

    let test_string_option () : unit =
      Quickcheck.test (gen_expr StringOption) ~f:(fun e ->
          match (I1.interp e, I2.interp e) with
          | ValStringOption s1, ValStringOption s2 ->
            [%test_eq: string option] s1 s2
          | _ -> failwith "failed bool")

    let run_tests () : unit =
      test_string_option ();
      printf "Mica: OK, passed %d observational equivalence tests.\n" 10000
  end
end

(******************************************************************************)
(* Using Mica to check observational equivalence of two implementations of
   finite maps below: *)

module T = Mica.TestHarness (AssocListMap) (RedBlackMap)

let () = T.run_tests ()
