(* Adatpted from code by Jean-Christophe Filliatre *)

open Polynomial_signature

module Poly1 : S = struct
  type monomial = { coeff : Base.Int.t; degree : Base.Int.t }
  [@@deriving fields, sexp]

  type t = monomial Base.List.t [@@deriving sexp]

  let zero = []
  let one = [ { coeff = 1; degree = 0 } ]
  let monomial c d = if c = 0 then [] else [ { coeff = c; degree = d } ]

  let rec add p1 p2 =
    match (p1, p2) with
    | [], _ -> p2
    | _, [] -> p1
    | m1 :: r1, m2 :: r2 ->
      if m1.degree > m2.degree then m1 :: add r1 p2
      else if m1.degree < m2.degree then m2 :: add p1 r2
      else
        let c = m1.coeff + m2.coeff in
        if c = 0 then add r1 r2
        else { coeff = c; degree = m1.degree } :: add r1 r2

  let create ml = List.fold_left (fun p (c, d) -> add (monomial c d) p) zero ml

  let mult p1 p2 =
    let mult_monomial m p =
      List.fold_right
        (fun m' acc ->
          let c = m.coeff * m'.coeff in
          if c = 0 then acc
          else { coeff = c; degree = m.degree + m'.degree } :: acc)
        p [] in
    List.fold_left (fun p m -> add (mult_monomial m p2) p) zero p1

  let rec power (x : int) (n : int) : int =
    if n == 0 then 1
    else
      let y = power x (n / 2) in
      if n mod 2 == 0 then y * y else x * y * y

  (* let eval (p : monomial list) (x : int) : int = match p with | [] -> 0 | _
     -> List.fold_left (fun acc m -> m.coeff * (power x m.degree) + acc) 0 p *)

  let rec eval (p : monomial list) (x : int) : int =
    match p with
    | [] -> 0
    | m :: r -> (m.coeff * power x m.degree) + eval r x

  let rec equal p1 p2 =
    match (p1, p2) with
    | [], [] -> true
    | [], _ | _, [] -> false
    | m1 :: r1, m2 :: r2 ->
      m1.degree = m2.degree && m1.coeff = m2.coeff && equal r1 r2

  let unknown =
    let u = ref (-1) in
    let rec gen u =
      if u < 26 then
        let c = if u < 3 then Char.code 'X' + u else Char.code 'A' + u - 3 in
        String.make 1 (Char.chr c)
      else gen ((u / 26) - 1) ^ gen (u mod 26) in
    fun () ->
      incr u;
      gen !u

  let var_name = unknown ()

  let rec print p =
    let rec print_aux = function
      | [] -> ()
      | { coeff = c; degree = n } :: r ->
        if (not (c = 1)) || n = 0 then Printf.fprintf stdout "(%d)" c;
        if n <> 0 then Printf.fprintf stdout "%s" var_name;
        if n > 1 then Printf.fprintf stdout "^%d" n;
        if r <> [] then Printf.fprintf stdout " + ";
        print_aux r in
    match p with
    | [] -> print zero
    | _ -> print_aux p
end
