open ECDHIntf
open Base_quickcheck

(** Suppress "unused-value" compiler warnings *)
[@@@ocaml.warning "-32"]

module G = Generator

(** Generates a hexadecimal digit *)  
let gen_hex_digit : char G.t = 
  Generator.(union [char_digit; char_uniform_inclusive 'a' 'f'])
  
(** Generates a hex string of length 64 *)  
let gen_hex_string : string G.t = 
  Generator.string_with_length_of ~length:32 gen_hex_digit

module ECDH_C : ECDHIntf = struct 
  include Callipyge
  let sexp_of_public_key = sexp_of_public_key
  let sexp_of_private_key = sexp_of_private_key
  
  let public_key_of_string  = public_key_of_string
  let private_key_of_string = private_key_of_string

  let public_key_of_private_key private_key = public_of_secret private_key

  let scalar_mult private_key public_key = 
    let out = Array.make 32 0 in 
    ecdh_inplace ~out ~secret:private_key ~public:public_key;
    public_key_of_int_array out

  let x25519_ecdh private_key public_key = 
    shared ~secret:private_key ~public:public_key |> string_of_public_key 

  (** QuickCheck generator for private keys *)
  let quickcheck_generator_private_key : private_key G.t = 
    let open G.Let_syntax in 
    gen_hex_string >>| private_key_of_string

  (** QuickCheck generator for public keys *)
  let quickcheck_generator_public_key : public_key G.t = 
    let open G.Let_syntax in 
    gen_hex_string >>| public_key_of_string
end   