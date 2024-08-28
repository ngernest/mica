open Regex_signature
open Dfa
open Brzozowski
open Base_quickcheck

(******************************************************************************)
(** The following is very similar to the code that Mica would generate
   automatically. The only manual modifications are: 
   - Variable renaming 
   - The use of the [char_alpha] QuickCheck generator to generate an alphabetic
     character (instead of any arbitrary ASCII character using 
     [quickcheck_generator_char]) *)

module Mica = struct
  type expr =
    | Void
    | Empty
    | Lit of char
    | Alt of expr * expr
    | Cat of expr * expr
    | Star of expr
    | MatchString of expr * string
    | AcceptsEmpty of expr

  type ty = Bool | T

  let rec gen_expr (ty : ty) : expr Generator.t =
    let open Generator in
    let open Let_syntax in
    let%bind k = size in
    match (ty, k) with
    | T, 0 -> union [ return Empty; return Void ]
    | Bool, _ ->
      let gen_matchString =
        let g1 = with_size ~size:(k / 2) (gen_expr T) in
        let g2 = quickcheck_generator_string in
        both g1 g2 >>| fun (e1, e2) -> MatchString (e1, e2) in
      let gen_acceptsEmpty =
        let g = with_size ~size:(k / 2) (gen_expr T) in
        g >>| fun e -> AcceptsEmpty e in
      union [ gen_matchString; gen_acceptsEmpty ]
    | T, _ ->
      let gen_lit =
        let g = char_alpha in
        g >>| fun e -> Lit e in
      let gen_alt =
        let g1 = with_size ~size:(k / 2) (gen_expr T) in
        let g2 = with_size ~size:(k / 2) (gen_expr T) in
        both g1 g2 >>| fun (e1, e2) -> Alt (e1, e2) in
      let gen_cat =
        let g1 = with_size ~size:(k / 2) (gen_expr T) in
        let g2 = with_size ~size:(k / 2) (gen_expr T) in
        both g1 g2 >>| fun (e1, e2) -> Cat (e1, e2) in
      let gen_start =
        let g = with_size ~size:(k / 2) (gen_expr T) in
        g >>| fun e -> Star e in
      union [ gen_lit; gen_alt; gen_cat; gen_start ]

  module Interpret (M : S) = struct
    include M

    type value = ValBool of bool | ValT of M.t

    let rec interp (expr : expr) : value =
      match expr with
      | Void -> ValT M.void
      | Empty -> ValT M.empty
      | Lit c -> ValT (M.lit c)
      | Alt (e1, e2) -> (
        match (interp e1, interp e2) with
        | ValT e1', ValT e2' -> ValT (M.alt e1' e2')
        | _ -> failwith "impossible")
      | Cat (e1, e2) -> (
        match (interp e1, interp e2) with
        | ValT e1', ValT e2' -> ValT (M.cat e1' e2')
        | _ -> failwith "impossible")
      | Star e -> (
        match interp e with
        | ValT e' -> ValT (M.star e')
        | _ -> failwith "impossible")
      | MatchString (e1, s2) -> (
        match interp e1 with
        | ValT e' -> ValBool (M.matchString e' s2)
        | _ -> failwith "impossible")
      | AcceptsEmpty e -> (
        match interp e with
        | ValT e' -> ValBool (M.acceptsEmpty e')
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

    let run_tests () : unit =
      test_bool ();
      printf "Mica: OK, passed %d observational equivalence tests.\n" 10000
  end
end

(******************************************************************************)
(* Using Mica to check observational equivalence of two implementations of
   regular expression matchers below: *)

module T = Mica.TestHarness (DFA) (Brzozowski)

let () = T.run_tests ()
