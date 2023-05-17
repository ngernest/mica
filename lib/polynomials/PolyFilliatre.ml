(**************************************************************************)
(*                                                                        *)
(*  Copyright (C) Jean-Christophe Filliatre                               *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2.1, with the special exception on linking            *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)

(* Polynomials over an abstract ring *)

open Format

module type Ring = sig
  type t
  val zero : t
  val one : t
  val add : t -> t -> t
  val neg : t -> t
  val mul : t -> t -> t

  val equal : t -> t -> bool

  val print : formatter -> t -> unit
end

let unknown =
  let u = ref (-1) in
  let rec gen u =
    if u < 26 then
      let c = if u < 3 then Char.code 'X' + u else Char.code 'A' + u - 3 in
      String.make 1 (Char.chr c)
    else
      gen (u / 26 - 1) ^ gen (u mod 26)
  in
  fun () -> incr u; gen !u

module type Polynomial = sig
  type r
  include Ring
  val monomial : r -> int -> t
  val create: (r * int) list -> t
  val var : t
  val sub : t -> t -> t
  val deg : t -> int
  val leading_coeff : t -> r
  type monomial = private { coef : r; degree : int; }
  val view : t -> monomial list
  val eval : t -> r -> r
end

module Make(X : Ring) = struct
  type r = X.t
  type monomial = { coef : X.t; degree : int } (* coef <> X.zero *)
  type t = monomial list                       (* sorted in decr. degrees *)
  let view p = p

  let zero = []
  let one = [ { coef = X.one; degree = 0 } ]
  let var = [ { coef = X.one; degree = 1 } ]

  let monomial c d =
    if X.equal c X.zero then [] else [{ coef = c; degree = d }]

  let deg = function
    | [] -> -1
    | { degree = n; _ } :: _ -> n

  let leading_coeff = function
    | [] -> X.zero
    | { coef = c; _ } :: _ -> c

  let rec add p1 p2 = match p1, p2 with
    | [], _ ->
        p2
    | _, [] ->
        p1
    | m1 :: r1, m2 :: r2 ->
        if m1.degree > m2.degree then
          m1 :: add r1 p2
        else if m1.degree < m2.degree then
          m2 :: add p1 r2
        else
          let c = X.add m1.coef m2.coef in
          if X.equal c X.zero then
            add r1 r2
          else
            { coef = c; degree = m1.degree } :: add r1 r2

  let create ml =
    List.fold_left (fun p (c, d) -> add (monomial c d) p) zero ml

  let neg = List.map (fun m -> { m with coef = X.neg m.coef })

  let sub p1 p2 = add p1 (neg p2)

  let mul p1 p2 =
    let mul_monomial m p =
      List.fold_right
        (fun m' acc ->
           let c = X.mul m.coef m'.coef in
           if X.equal c X.zero then acc
           else { coef = c; degree = m.degree + m'.degree } :: acc)
        p []
    in
    List.fold_left (fun p m -> add (mul_monomial m p2) p) zero p1

  let rec power x n =
    if n == 0 then
      X.one
    else
      let y = power x (n / 2) in
      if n mod 2 == 0 then X.mul y y else X.mul x (X.mul y y)

  let rec eval p x = match p with
    | [] -> X.zero
    | m :: r -> X.add (X.mul m.coef (power x m.degree)) (eval r x)

  (* TODO: use a (tail-recursive) Horner schema *)

  let rec equal p1 p2 = match p1, p2 with
    | [], [] ->
        true
    | [], _ | _, [] ->
        false
    | m1 :: r1, m2 :: r2 ->
        m1.degree = m2.degree && X.equal m1.coef m2.coef && equal r1 r2

  let var_name = unknown ()

  let print fmt p =
    let rec print_aux = function
      | [] ->
          ()
      | { coef = c; degree = n } :: r ->
          if not (X.equal c X.one) || n = 0 then fprintf fmt "(%a)" X.print c;
          if n <> 0 then fprintf fmt "%s" var_name;
          if n > 1 then fprintf fmt "^%d" n;
          if r <> [] then fprintf fmt " + ";
          print_aux r
    in
    match p with
      | [] -> X.print fmt X.zero
      | _ -> print_aux p

end

