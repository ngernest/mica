(** This library provides the two Diffie-Hellman-like functions defined in the
    eponymous RFC, [x25519] and [x448]. *)

(** {2 Summary} *)

(** X25519 and X448 are instances of a special subset of elliptic curves, the
    so-called {e Edwards curves}, for which point addition has a closed form.
    This eliminates a whole class of problems that arise in other elliptic curve
    implementations, where addition formulas depend on the arguments (e.g.
    whether a point is added to itself). In addition, these curves are also
    {e designed} to be safe to implement and use: the addition formula is by
    construction resistant to timing attacks, neither public keys nor private
    keys need to be validated and the string-based interface is very portable. *)

(** {2 Quick Start}

    You can use {!Rfc7748.x25519} and {!Rfc7748.x448} as described in the example
    program in the source tree. *)

(** {2 API}

    Below is the public API for this library. It is divided into
    - a module type {!Rfc7748.DH} for scalar multiplication on Edwards curves
      (the elliptic curve analogon of discrete exponentiation, as used in the
      classical Diffie-Hellman key exchange)
    - the curves {!Rfc7748.X25519} and {!Rfc7748.X448} and
    - the functions {!Rfc7748.x25519} and {!Rfc7748.x448} described in RFC 7748.
*)


(** Signature of the modules implementing the Diffie-Hellman functions for
    RFC 7748. 
*)
module type DH = sig

  (** {2 Types} *)

  type private_key
  (** A private key for this curve. Private keys are, generally speaking, natural
      numbers for scalar multiplication of curve points. *)

  type public_key
  (** A public key for this curve. Public keys form a curve in 2D space and are
      uniquely identified by their x-coordinate (up to symmetry). *)


  (** {2 Constants} *)

  val key_size: int
  (** The size of a valid private or public key, in bytes. *)

  val base: public_key
  (** The base point of the curve. *)

  (** {2 Key Conversion} *)

  val public_key_of_string: string -> public_key
  (** Create a public key from a given string. The key is assumed to be encoded in
      hexadecimal and must have a length of [2*key_size]. The string is silently
      truncated (if too long) or padded (if too short). An exception is raised if
      the string contains characters that are not valid hexadecimal digits. The
      curves in RFC 7748 are specially crafted such that public keys received
      from untrusted sources can be used safely without validation.

      Note that the creation includes a canonicalization step and thus, in
      general, [string_of_public_key (public_key_of_string s) <> s].*)

  val private_key_of_string: string -> private_key
  (** Create a private key from a given string. The key is assumed to be encoded in
      hexadecimal and must have a length of [2*key_size]. The string is silently
      truncated (if too long) or padded (if too short). An exception is raised if
      the string contains characters that are not valid hexadecimal digits. The
      curves in RFC 7748 are specially crafted such that any random sequence of
      bytes of the correct length can be used to create a private key.

      Note that the creation includes a canonicalization step and thus, in
      general, [string_of_private_key (private_key_of_string s) <> s].*)

  val string_of_public_key: public_key -> string
  (** Convert the public key to its hex-encoded string representation. *)

  val string_of_private_key: private_key -> string
  (** Convert the private key to its hex-encoded string representation. *)

  val public_key_of_bytes: Bytes.t -> public_key
  (** Create a public key from a byte array of length [key_size]. An exception
      is raised if the array has the wrong size. Besides that, the function
      behaves like {!public_key_of_string} after hex-decoding. The array is not
      modified by this module, and it must not be modified externally while the
      [public_key] object is in scope. *)

  val private_key_of_bytes: Bytes.t -> private_key
  (** Create a private key from a byte array of length [key_size]. An exception
      is raised if the array has the wrong size. Besides that, the function
      behaves like {!private_key_of_string} after hex-decoding. The array is not
      modified by this module, and it must not be modified externally while the
      [public_key] object is in scope. *)

  val bytes_of_public_key: public_key -> Bytes.t
  (** Create a byte array from the given public key. *)

  val bytes_of_private_key: private_key -> Bytes.t
  (** Create a byte array from the given private key. *)

  (** {2 Curve Operations} *)

  val scale: private_key -> public_key -> public_key
  (** [scale priv pub] is the scalar multiplication of [pub] by [priv]. *)

  val public_key_of_private_key: private_key -> public_key
  (** [public_key_of_private_key priv] is equal to [scale priv base] *)
end

(** X25519 (based on Curve25519 by Daniel J. Bernstein)

    This curve is considered to have a strength equivalent to 128 bit in a
    symmetric cipher. It was proposed in
    {{:https://cr.yp.to/ecdh/curve25519-20060209.pdf} Curve25519} by Daniel J.
    Bernstein. *)
module X25519: DH

(** X448 (based on Ed448-Goldilocks by Mike Hamburg)

    This curve is considered to have a strength equivalent to 224 bit in a
    symmetric cipher. It was proposed in
    {{:https://eprint.iacr.org/2015/625.pdf} Ed448-Goldilocks} by Mike
    Hamburg. *)
module X448: DH

val x25519: priv:string -> pub:string -> string
(** This is a shortcut for using [X25519.scale] without converting keys. *)

val x448: priv:string -> pub:string -> string
(** This is a shortcut for using [X448.scale] without converting keys. *)
