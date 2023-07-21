module type ECDHIntf = sig
  type public_key
    [@@deriving sexp_of]
  type private_key
    [@@deriving sexp_of]
  val base : public_key
  val public_key_of_string : string -> public_key
  val private_key_of_string : string -> private_key
  val string_of_public_key : public_key -> string
  val string_of_private_key : private_key -> string
  val public_key_of_private_key : private_key -> public_key
  val scalar_mult : private_key -> public_key -> public_key
  val x25519_ecdh : private_key -> public_key -> string
end