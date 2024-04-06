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
[@@deriving_inline mica_types, mica]


include
  struct
    [@@@ocaml.warning "-60"]
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
    module ExprToImpl(M:SetInterface) =
      struct
        include M
        type value =
          | ValBool of bool
          | ValInt of int
          | ValIntT of int t
        let rec interp e =
          match e with
          | Empty -> M.empty
          | Is_empty e1 -> (match interp e1 with | _ -> 1)
          | Mem (n1, e2) -> (match interp e2 with | _ -> 1)
          | Add (n1, e2) -> (match interp e2 with | _ -> 1)
          | Rem (n1, e2) -> (match interp e2 with | _ -> 1)
          | Size e1 -> (match interp e1 with | _ -> 1)
          | Union (e1, e2) ->
              (match ((interp e1), (interp e2)) with | _ -> 1)
          | Intersect (e1, e2) ->
              (match ((interp e1), (interp e2)) with | _ -> 1)
          | Invariant e1 -> (match interp e1 with | _ -> 1)
        let _ = interp
      end
  end[@@ocaml.doc "@inline"]
[@@@end]
