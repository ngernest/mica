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
(* [@@deriving_inline mica_types, mica]

include
  struct
    [@@@ocaml.warning "-60"]
    type expr =
      | Void
      | Empty
      | Lit of char
      | Alt of expr * expr
      | Cat of expr * expr
      | Star of expr
      | MatchString of expr * string
      | AcceptsEmpty of expr
    type ty =
      | Bool
      | T
    module TestHarness(M:RegexMatcher) =
      struct
        include M
        type value =
          | ValBool of bool
          | ValT of t
        let rec interp e =
          match e with
          | Void -> ValT M.void
          | Empty -> ValT M.empty
          | Lit c1 ->
              (match interp c1 with
               | ValT c1' -> ValT (M.lit c1')
               | _ -> failwith "impossible")
          | Alt (e1, e2) ->
              (match ((interp e1), (interp e2)) with
               | (ValT e1', ValT e2') -> ValT (M.alt e1' e2')
               | _ -> failwith "impossible")
          | Cat (e1, e2) ->
              (match ((interp e1), (interp e2)) with
               | (ValT e1', ValT e2') -> ValT (M.cat e1' e2')
               | _ -> failwith "impossible")
          | Star e1 ->
              (match interp e1 with
               | ValT e1' -> ValT (M.star e1')
               | _ -> failwith "impossible")
          | MatchString (e1, s2) ->
              (match interp e1 with
               | ValT e1' -> ValBool (M.matchString e1' s2)
               | _ -> failwith "impossible")
          | AcceptsEmpty e1 ->
              (match interp e1 with
               | ValT e1' -> ValBool (M.acceptsEmpty e1')
               | _ -> failwith "impossible")
        let _ = interp
      end
  end[@@ocaml.doc "@inline"]
[@@@end] *)

