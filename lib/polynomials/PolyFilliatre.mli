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

(** Polynomials over an abstract ring.

    This module implements polynomials with coefficients in an arbitrary 
    ring, as a functor. The resulting module also implements a ring, and
    thus multiple applications yield polynomials in several variables.
*)

module type Ring = sig
  type t
  val zero : t
  val one : t
  val add : t -> t -> t
  val neg : t -> t
  val mul : t -> t -> t
  val equal : t -> t -> bool
  val print : Format.formatter -> t -> unit
end

module type Polynomial = sig
  type r
    (** The type of the coefficients (a ring); in the following,
        we note [r0] and [r1] the zero and one values of this ring. *)

  include Ring
    (** The ring of polynomials *)
    (** Ring constants and operations over the polynomials. *)

  val monomial : r -> int -> t
    (** [monomial r n] creates a monomial of coefficient [r] and degree [n]. *)

  val create: (r * int) list -> t

  val var : t
    (** The variable; same as [monomial r1 1]. *)

  val sub : t -> t -> t
    (** for convenience *)

  val deg : t -> int
    (** The degree of a polynomial; returns [-1] if the polynomial is zero. *)
  val leading_coeff : t -> r
    (** Leading coefficient; returns [r0] if the polynomial is zero. *)

  type monomial = private { coef : r; degree : int }
  val view : t -> monomial list
    (** A polynomial as a list of monomials, in decreasing order of degrees,
        with non-zero coefficients. *)

  val eval : t -> r -> r
    (** [eval p v] evaluates [p(v)] *)

end

