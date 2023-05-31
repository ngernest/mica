(* Adatpted from code by Shayne Fletcher *)

(* Disable "unused-values" compiler warnings *)
[@@@ocaml.warning "-32-34-27"]

open PolyInterface

module Poly2 : PolyInterface = struct
  (* Invariant : Ordered by powers, lower order terms at the front *)
  type t = (Base.Int.t * Base.Int.t) Base.List.t 
    [@@deriving sexp]

  let print p =
    List.iter
      (fun (c, k) -> Printf.printf "+ (";
        Printf.printf "%d" c;
        Printf.printf ")X^%d " k)
      p
  
  let zero = []

  let one = [(1, 0)]

  let monomial (a : int) (k : int) =
    if k < 0 then
      failwith "monomial : negative powers not supported"
    else if a = 0 then [] else [(a, k)]

  let rec add u v =
    match u, v with
    | [], _ -> v
    | _, [] -> u
    | ((c1, k1) :: r1 as p1), ((c2, k2) :: r2 as p2) ->
      if k1 < k2 then
        (c1, k1) :: (add r1 p2)
      else if k1 = k2 then
        let c = c1 + c2 in
        if c = 0 then add r1 r2
        else (c, k1) :: (add r1 r2)
      else (c2, k2) :: (add p1 r2)

  let create l =
    List.fold_left (fun acc (c, k) ->
      add (monomial c k) acc) zero l

  let rec times (c, k) = function
  | [] -> []
  | (c1, k1) :: q ->
    let c2 = c * c1 in
    if c2 = 0 then times (c, k) q
    else (c2, k + k1) :: times (c, k) q

  let mult p = List.fold_left (fun r m -> add r (times m p)) zero

  let rec equal p1 p2 =
    match p1, p2 with
    | [], [] -> true
    | (c1, k1) :: q1, (c2, k2) :: q2 ->
      k1 = k2 && c1 = c2 && equal q1 q2
    | _ -> false

  let rec pow c = function
  | 0 -> 1
  | 1 -> c
  | k ->
    let l = pow c (k lsr 1) in
    let l2 = l * l in
    if k land 1 = 0 then l2 else c * l2

  let eval p c = match List.rev p with
  | [] -> 0
  | (h :: t) ->
    let reduce (a, k) (b, l) =
      let n = pow c (k - l) in
      let t = (a * n) + b in
      (t, l)  in
    let a, k = List.fold_left reduce h t in
    (pow c k) * a
end