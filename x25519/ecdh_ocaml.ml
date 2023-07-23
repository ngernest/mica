open Rfc7748
open ECDHIntf
open Base_quickcheck

(** Suppress "unused-value" compiler warnings *)
[@@@ocaml.warning "-32"]

module G = Generator

(** Generates a hexadecimal digit *)  
let gen_hex_digit : char Generator.t = 
  Generator.(union [char_digit; char_uniform_inclusive 'a' 'f'])
  
(** Generates a hex string of length 64 *)  
let gen_hex_string : string Generator.t = 
  Generator.string_with_length_of ~length:64 gen_hex_digit


(** Implementation of Elliptic-Curve Diffie-Hellman in OCaml *)
module ECDH_OCaml : ECDHIntf = struct 
  include X25519
  
  let scalar_mult = scale 

  let x25519_ecdh (priv_k : private_key) (pub_k : public_key) : string = 
    scalar_mult priv_k pub_k |> string_of_public_key

  let shared_secret_alice (alice : string) (bob : string) (base : string) : string = 
    x25519 ~priv:alice ~pub:(x25519 ~priv:bob ~pub:base)

  let shared_secret_bob (bob : string) (alice : string) (base : string) : string = 
    x25519 ~priv:bob ~pub:(x25519 ~priv:alice ~pub:base)   

  (** QuickCheck generator for private keys *)
  let quickcheck_generator_private_key : private_key G.t = 
    let open G.Let_syntax in 
    gen_hex_string >>| private_key_of_string

  (** QuickCheck generator for public keys *)
  let quickcheck_generator_public_key : public_key G.t = 
    let open G.Let_syntax in 
    gen_hex_string >>| public_key_of_string

  (** Deprecated implementation *)
  (* let quickcheck_generator_private_key : private_key G.t =   
    (* Generate a byte array of length 32, where each byte corresponds to 
        an alphanumeric character *)
    let bytes = Bytes.init 32 
      ~f:(fun _ -> Core.Quickcheck.random_value G.char_alphanum) in 
    G.return @@ private_key_of_bytes bytes *)
end  

