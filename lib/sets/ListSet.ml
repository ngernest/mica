open SetInterface

module ListSet : SetInterface = struct 
  (** AF: The list [a1; ...; an] represents the set {a1, ..., an}. 
          The empty list [[]] represents the empty set. 
      RI: The list must not contain duplicates. *)
  type 'a t = 'a Base.List.t 
    [@@deriving sexp, equal]

  let empty = []

  let is_empty lst = lst == []
  
  let mem x lst = List.mem x lst
  
  let add x s = 
    if mem x s then s else (x :: s)
  
  let rem x = List.filter (( <> ) x)
  
  let dedup lst = 
    lst |> List.sort_uniq Stdlib.compare

  let size s = 
      s |> dedup |> List.length
  
  let union s1 s2 = 
    s1 @ s2 |> dedup
  
  let intersect lst1 lst2 = 
      List.filter (fun h -> mem h lst2) lst1

  (* Invariant: list can only contain unique elements *)
  let invariant s = 
    let open Base in 
    not @@ List.contains_dup ~compare:Poly.compare s
end
