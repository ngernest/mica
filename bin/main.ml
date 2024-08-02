type expr =
  | Empty
  | Is_empty of expr
  | Mem of int * expr
  | Add of int * expr
  | Rem of int * expr
  | Size of expr
  | Union of expr * expr
  | Intersect of expr * expr
[@@deriving show { with_path = false }]

let rec depth (acc : int) (e : expr) : int =
  match e with
  | Empty -> acc
  | Is_empty e | Mem (_, e) | Add (_, e) | Rem (_, e) | Size e ->
    depth (1 + acc) e
  | Union (e1, e2) | Intersect (e1, e2) ->
    let x1 = depth (1 + acc) e1 in
    let x2 = depth (1 + acc) e2 in
    Int.max x1 x2

let rec fold (f : 'a -> 'a) (acc : 'a) (e : expr) : 'a =
  match e with
  | Empty -> acc
  | Is_empty e | Mem (_, e) | Add (_, e) | Rem (_, e) | Size e ->
    f (fold f acc e)
  | Union (e1, e2) | Intersect (e1, e2) -> f (fold f (fold f acc e2) e1)

let incr x = x + 1

let () =
  (* depth = 2 *)
  let res = depth 0 (Add (5, Add (3, Empty))) in
  Stdio.printf "%d\n" res
