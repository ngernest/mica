(* Implementation by Harry Goldstein, Dec 2022 *)
open! Core

module type S = sig
  type value
  type priority
  type t

  val empty : t
  val insert : t -> value:value -> priority:priority -> t
  val extract : t -> (priority * value * t) option
end

(** Priority queue implemented as a map from P to V *)
module M (P : Comparable) (V : Comparable) :
  S with type priority = P.t and type value = V.t = struct
  type value = V.t
  type priority = P.t

  (* Map from P.t to V.t list*)
  type t = V.t list Map.M(P).t

  let empty = Map.empty (module P)

  let insert q ~value ~priority =
    Map.update q priority ~f:(function
      | None -> [ value ]
      | Some l -> List.dedup_and_sort ~compare:V.compare (value :: l))

  let extract q =
    let open Option.Let_syntax in
    let%map v = Map.max_elt q in
    match v with
    | _, [] -> assert false
    | priority, value :: [] -> (priority, value, Map.remove q priority)
    | priority, value :: rest ->
        (priority, value, Map.update q priority ~f:(fun _ -> rest))
end