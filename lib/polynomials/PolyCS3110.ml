(**
Source: Cornell CS 3110 Course Staff
https://github.com/cs3110/textbook-solutions/blob/main/chapter6a.ml
*)

(********************************************************************
 * exercise: poly spec
 ********************************************************************)

(* this is a sample solution; others are possible *)

(* [Poly] represents immutable polynomials with integer coefficients. *)
module type Poly = sig
  (* [t] is the type of polynomials *)
  type t

  (* [eval x p] is [p] evaluated at [x].  For example, if [p] represents
   * $3x^3 + x^2 + x$, then evaluating [p] at [10] would yield [3110]. *)
  val eval : int -> t -> int


  (* # Construction *)

  (* [from_coefficients [c0, ..., cn]] constructs a new polynomial [p] where
   *  [p] represents the polynomial $c_n x^n + ... + c_1 x + c_0$.
  *)
  val from_coefficients : int list -> t


  (* # Querying *)

  (* [coefficient i p] returns the coefficient [c_i] of the term with power
   *   [i]. That is, if $p = c_n x^n + ... + c_i x^i + ... + c_0$, then
   *   [coefficient i p = c_i]. If $i > degree p$, then [coefficient i p = 0].
   * requires: i >= 0
  *)
  val coefficient : int -> t -> int

  (* [degree p] returns the largest [i] such that [coefficient i p <> 0].
   * The result is [-1] if there is no such [i].
  *)
  val degree : t -> int


  (* # Operations *)

  (* [scale c p] returns [p'] such that
   * [coefficient i p' = c * coefficient i p] for all [i >= 0].
  *)
  val scale : int -> t -> t

  (* [add p1 p2] returns [p] such that
   * [coefficient i p = coefficient i p1 + coefficient i p2] for all [i >= 0].
  *)
  val add : t -> t -> t

  (* [multiply p1 p2] returns [p] such that
   * [coefficient i p = sum [ coefficient j p1 * coefficient (i - j) p2 | 0 <= j <= i]]
   * for all [i >= 0].
  *)
  val multiply : t -> t -> t
end


(********************************************************************
 * exercise: poly impl
 ********************************************************************)

module PolyImpl : Poly = struct
  (* [c0; c1; ...; cn] represents the polynomial
   *  $c0 + c1 x + ... + cn x^n$. The empty list represents 0,
   *  as does [0], and [0;0], etc. *)
  type t = int list

  let rec eval x = function
    | [] -> 0
    | c :: rest -> c + x * eval x rest


  let from_coefficients p =
    p


  let coefficient i p =
    if i < List.length p then List.nth p i else 0

  let degree p =
    (* [drop_while p l] is [l] with its longest prefix satisfying
     * [p] is removed. We use this to remove leading zeros from the
     * coefficient list.
    *)
    let rec drop_while f = function
      | [] -> []
      | x :: xs -> if f x then drop_while f xs else x :: xs
    in
    List.length (drop_while (fun x -> x = 0) (List.rev p)) - 1


  (* [scale c p] returns [p'] such that
   * [coefficient i p' = c * coefficient i p] for all [i >= 0].
  *)
  let scale c =
    List.map (fun c' -> c * c')

  let rec add p1 p2 =
    match p1, p2 with
    | [], _ -> p2 (* empty list is the 0 polynomial *)
    | _, [] -> p1 (* empty list is the 0 polynomial *)
    | c1 :: p1, c2 :: p2 -> c1 + c2 :: add p1 p2

(*
 * Multiplying a polynomial [p1] with degree [d1] and [p2] with
 * degree [d2] will generate a polynomial with degree at most
 * [d1 + d2], that is, the highest power of $x$ will be at most
 * $x^(d1 + d2)$ (zero polynomial multiplied with any polynomial
 * is still the zero polynomial with degree -1; that's why we say
 * at most). Rather than thinking about the entire polynomial that
 * is the results of the multiplication, we will think about how we can
 * generate a specific coefficient [c_i] for the [i]th power of
 * [x]. Then it is easy to generate all coefficients
 * [c_0, ..., c_(d1 + s2)] one at a time.
 *
 * To get [i] copies of [x] (that is, [x^i]) we need to select
 * all coefficients [c_j] from [p1] and [c_k] from [p2] such that
 * [j + k = i]. For example, if we want [x^3], we can get it by
 * multiplying [x] with [x^2] or [1] with [x^3] and so on. The problem
 * then becomes identifying all such pairs of coefficients, multiplying
 * them together and adding up the results.
 *)
  let multiply p1 p2 =
    (* [tabulate f s n] returns a new list of length [n] where the [i]-th
     * element in the list is [f (s + i)]. [s] serves as a starting value.
     * If we give [tabulate] the function that generates the [i]th coefficient
     * of the result, and specify a suitable bound, we will get all the
     * coefficients that correspond to the multiplication.
     * requires: [s >= 0]
    *)
    let rec tabulate f s = function
      | n when n <= 0 -> []
      | n -> f s :: tabulate f (s + 1) (n - 1)
    in

    (* [take l n] returns the first [n] elements of [l]. If [l] doesn't
     * have enough elements, the tail of the result is padded with [0]s.
    *)
    let rec take l n =
      match l, n with
      | _      , 0 -> []
      | []     , n -> 0 :: take [] (n - 1)
      | x :: xs, n -> x :: take xs (n - 1)
    in

    (* [dotProduct [x_1; ...; x_n] [y_1; ...; y_n]] is
     * [sum [x_1 * y_1; ...; x_n * y_n]].
    *)
    let rec dotProduct l1 l2 =
      match l1, l2 with
      | [], _ -> 0
      | _, [] -> 0
      | x1 :: l1, x2 :: l2 -> x1 * x2 + dotProduct l1 l2
    in

    (* [ith] is the [i]th coefficient of the resulting polynomial.
       requires: [i >= 0]
    *)
    let ith i =
      (* All coefficients of [p1] (respectively p2) up to and including
       * the coefficient of [x^i].
      *)
      let p1_up_to_ith_power = take p1 (i + 1) in
      let p2_up_to_ith_power = take p2 (i + 1) in
      (* Match each coefficient [c_j] in [p1] with coefficient
       * [c_k = c_(i - j)] in [p2]. By definition, this ensures
       * [j + k = i] and these are the only such coefficients.
       * Multiply the pairs together and the results up (i.e.
       * take the [dotProduct].) This is the [i]th coefficient
       * by the specification of multiplication.
      *)
      dotProduct p1_up_to_ith_power (List.rev p2_up_to_ith_power)
    in
    tabulate ith 0 (degree p1 + degree p2 + 1)
end