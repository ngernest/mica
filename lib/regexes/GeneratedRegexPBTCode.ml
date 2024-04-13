open Base
(** Auto-generated property-based testing code *)

open Base_quickcheck
open RegexMatcher
open Brzozowski
open DFA

(* Suppress "unused value" compiler warnings *)
[@@@ocaml.warning "-27-32-33-34"]

type expr =
  | Void
  | Empty
  | Lit of char
  | Alt of expr * expr
  | Cat of expr * expr
  | Star of expr
  | MatchString of expr * string
  | AcceptsEmpty of expr
[@@deriving sexp_of, compare, equal, to_yojson { exn = true }]

type ty = Bool | T [@@deriving sexp_of, compare]

(** Module needed to create a [Map] from [ty]'s to pre-generated [value]'s *)
module Ty = struct
  module T = struct
    type t = ty [@@deriving compare, sexp_of]
  end

  include T
  include Comparable.Make (T)
end

module ExprToImpl (M : RegexMatcher) = struct
  include M

  type value = ValBool of bool | ValT of M.t [@@deriving sexp_of]

  type bank = value list Map.M(Ty).t
  (** A [bank] is a map from base types ([ty]'s) 
      to a list of pre-generated [value]'s *)

  (* let gen_bank () : bank =
     let seed = `Nondeterministic in
     let length = 5 in failwith "TODO" *)

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

let rec gen_expr (ty : ty) : expr Generator.t =
  let module G = Generator in
  let open G.Let_syntax in
  let%bind k = G.size in
  match (ty, k) with
  | T, 0 -> G.union [ G.return Empty; G.return Void ]
  | Bool, _ ->
      let matchString =
        let%bind e1 = G.with_size ~size:(k / 2) (gen_expr T) in
        let%bind s2 = G.string_non_empty in
        G.return @@ MatchString (e1, s2)
      in
      let acceptsEmpty =
        let%bind e = G.with_size ~size:(k / 2) (gen_expr T) in
        G.return @@ AcceptsEmpty e
      in
      G.union [ matchString; acceptsEmpty ]
  | T, _ ->
      let lit =
        let%bind c = G.char_alpha in
        G.return @@ Lit c
      in
      let alt =
        let%bind e1 = G.with_size ~size:(k / 2) (gen_expr T) in
        let%bind e2 = G.with_size ~size:(k / 2) (gen_expr T) in
        G.return @@ Alt (e1, e2)
      in
      let cat =
        let%bind e1 = G.with_size ~size:(k / 2) (gen_expr T) in
        let%bind e2 = G.with_size ~size:(k / 2) (gen_expr T) in
        G.return @@ Cat (e1, e2)
      in
      let star =
        let%bind e = G.with_size ~size:(k / 2) (gen_expr T) in
        G.return @@ Star e
      in
      G.union [ lit; alt; cat; star ]

module I1 = ExprToImpl (Brzozowski)
module I2 = ExprToImpl (DFA)

let displayError (e : expr) (v1 : I1.value) (v2 : I2.value) : string =
  Printf.sprintf "e = %s, v1 = %s, v2 = %s\n"
    (Sexp.to_string @@ sexp_of_expr e)
    (Sexp.to_string @@ [%sexp_of: I1.value] v1)
    (Sexp.to_string @@ [%sexp_of: I2.value] v2)
