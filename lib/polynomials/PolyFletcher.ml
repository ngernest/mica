(**
Implementation by Shayne Fletcher
https://blog.shaynefletcher.org/2017/03/polynomials-over-rings.html    
*)

module type ARITH = sig
  type t
  val of_int : int -> t            
  val to_int : t -> int
  val of_string : string -> t      
  val to_string : t -> string
  val zero : t                     
  val one : t
  val add : t -> t -> t            
  val sub : t -> t -> t
  val mul : t -> t -> t            
  val div : t -> t -> t
  val compare : t -> t -> int      
  val equal : t -> t -> bool
end


module type RING = sig
  type t
  type extern_t
  val print : t -> unit
  val make : extern_t -> t         
  val show : t -> extern_t
  val zero : t                     
  val one : t
  val add : t -> t -> t            
  val mul : t -> t -> t
  val equal : t -> t -> bool
end

module Ring (A : ARITH) :
  RING with type t = A.t and type extern_t = int =
struct
  include A
  type extern_t = int
  let make = of_int                let show = to_int
  let print x = print_int (show x)
end

module type POLYNOMIAL = sig
  type coeff (*Type of coefficients*)
  type coeff_extern_t (*Type of coeff. external rep*)

  (*Polynomials satisfy the ring interface*)
  include RING (*Declares a type [t] and [extern_t]*)

  (*Function to evaluate a polynomial at a point*)
  val eval : t -> coeff -> coeff
end

module Polynomial (R : RING) :
      POLYNOMIAL with type coeff = R.t
      and type coeff_extern_t = R.extern_t
      and type extern_t = (R.extern_t * int) list =
struct

  type coeff = R.t (*Coefficient type*)
  type coeff_extern_t = R.extern_t (*External coeff. rep*)
  type extern_t = (coeff_extern_t * int) list (*External polynomial rep*)

  (*List of coefficients and their powers*)
  type t = (coeff * int) list (*Invariant : Ordered by powers,
                                lower order terms at the front*)

  let print p =
    List.iter
      (fun (c, k) -> Printf.printf "+ (";
        R.print c;
        Printf.printf ")X^%d " k)
      p
  
  let zero = []

  let one = [R.one, 0]

  let monomial (a : coeff) (k : int) =
    if k < 0 then
      failwith "monomial : negative powers not supported"
    else if R.equal a R.zero then [] else [a, k]

  let rec add u v =
    match u, v with
    | [], _ -> v
    | _, [] -> u
    | ((c1, k1) :: r1 as p1), ((c2, k2) :: r2 as p2) ->
      if k1 < k2 then
        (c1, k1) :: (add r1 p2)
      else if k1 = k2 then
        let c = R.add c1 c2 in
        if R.equal c R.zero then add r1 r2
        else (c, k1) :: (add r1 r2)
      else (c2, k2) :: (add p1 r2)

  let make l =
    List.fold_left (fun acc (c, k) ->
      add (monomial (R.make c) k) acc) zero l

  let show p =
    List.fold_right (fun (c, k) acc -> (R.show c, k) :: acc) p []

  let rec times (c, k) = function
  | [] -> []
  | (c1, k1) :: q ->
    let c2 = R.mul c c1 in
    if R.equal c2 R.zero then times (c, k) q
    else (c2, k + k1) :: times (c, k) q

  let mul p = List.fold_left (fun r m -> add r (times m p)) zero

  let rec equal p1 p2 =
    match p1, p2 with
    | [], [] -> true
    | (c1, k1) :: q1, (c2, k2) :: q2 ->
      k1 = k2 && R.equal c1 c2 && equal q1 q2
    | _ -> false

  let rec pow c = function
  | 0 -> R.one
  | 1 -> c
  | k ->
    let l = pow c (k lsr 1) in
    let l2 = R.mul l l in
    if k land 1 = 0 then l2 else R.mul c l2

  let eval p c = match List.rev p with
  | [] -> R.zero
  | (h :: t) ->
    let reduce (a, k) (b, l) =
      let n = pow c (k - l) in
      let t = R.add (R.mul a n) b in
      (t, l)  in
    let a, k = List.fold_left reduce h t in
    R.mul (pow c k) a
end

(* Ring of integers *)
module Ring_int = Ring (struct
  type t = int
  let of_int x = x                   
  let to_int x = x
  let of_string = int_of_string      
  let to_string = string_of_int
  let zero = 0                       
  let one = 1
  let add = ( + )                    
  let sub = ( - )
  let mul = ( * )                    
  let div = ( / )
  let compare = Base.Int.compare   
  let equal = ( = )
end)

(** Polynomials over the ring of integers *)
module Polynomial_int = Polynomial(Ring_int)