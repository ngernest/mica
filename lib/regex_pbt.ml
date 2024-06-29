(** Auto-generated property-based testing code *)
open Base

open Base_quickcheck

module type RegexMatcher = sig
  type t

  val void : t

  val empty : t

  val lit : char -> t

  val alt : t -> t -> t

  val cat : t -> t -> t

  val star : t -> t

  val matchString : t -> string -> bool

  val acceptsEmpty : t -> bool
end

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

type ty = Bool | T

module ExprToImpl (M : RegexMatcher) = struct
  include M

  type value = ValBool of bool | ValT of M.t

  let rec interp (expr : expr) : value =
    match expr with
    | Void ->
        ValT M.void
    | Empty ->
        ValT M.empty
    | Lit c ->
        ValT (M.lit c)
    | Alt (e1, e2) -> (
      match (interp e1, interp e2) with
      | ValT e1', ValT e2' ->
          ValT (M.alt e1' e2')
      | _ ->
          failwith "impossible" )
    | Cat (e1, e2) -> (
      match (interp e1, interp e2) with
      | ValT e1', ValT e2' ->
          ValT (M.cat e1' e2')
      | _ ->
          failwith "impossible" )
    | Star e -> (
      match interp e with
      | ValT e' ->
          ValT (M.star e')
      | _ ->
          failwith "impossible" )
    | MatchString (e1, s2) -> (
      match interp e1 with
      | ValT e' ->
          ValBool (M.matchString e' s2)
      | _ ->
          failwith "impossible" )
    | AcceptsEmpty e -> (
      match interp e with
      | ValT e' ->
          ValBool (M.acceptsEmpty e')
      | _ ->
          failwith "impossible" )
end

let rec gen_expr (ty : ty) : expr Generator.t =
  let open Generator in
  let open Let_syntax in
  let%bind k = size in
  match (ty, k) with
  | T, 0 ->
      union [return Empty; return Void]
  | Bool, _ ->
      let gen_matchString =
        let g1 = with_size ~size:(k / 2) (gen_expr T) in
        let g2 = string_non_empty in
        both g1 g2 >>| fun (e1, e2) -> MatchString (e1, e2)
      in
      let gen_acceptsEmpty =
        let g = with_size ~size:(k / 2) (gen_expr T) in
        g >>| fun e -> AcceptsEmpty e
      in
      union [gen_matchString; gen_acceptsEmpty]
  | T, _ ->
      let gen_lit =
        let g = char_alpha in
        g >>| fun e -> Lit e
      in
      let gen_alt =
        let g1 = with_size ~size:(k / 2) (gen_expr T) in
        let g2 = with_size ~size:(k / 2) (gen_expr T) in
        both g1 g2 >>| fun (e1, e2) -> Alt (e1, e2)
      in
      let gen_cat =
        let g1 = with_size ~size:(k / 2) (gen_expr T) in
        let g2 = with_size ~size:(k / 2) (gen_expr T) in
        both g1 g2 >>| fun (e1, e2) -> Cat (e1, e2)
      in
      let gen_start =
        let g = with_size ~size:(k / 2) (gen_expr T) in
        g >>| fun e -> Star e
      in
      union [gen_lit; gen_alt; gen_cat; gen_start]
