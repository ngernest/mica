open Rfc7748
open ECDHIntf
open Core 
open Core.Quickcheck

(** Suppress "unused-value" compiler warnings *)
[@@@ocaml.warning "-32"]


module G = Generator

module ECDH_OCaml : ECDHIntf = struct 
  include X25519
  
  let scalar_mult = scale 

  let x25519_ecdh (priv_k : private_key) (pub_k : public_key) : string = 
    scalar_mult priv_k pub_k |> string_of_public_key

  let shared_secret_alice (alice : string) (bob : string) (base : string) : string = 
    x25519 ~priv:alice ~pub:(x25519 ~priv:bob ~pub:base)

  let shared_secret_bob (bob : string) (alice : string) (base : string) : string = 
    x25519 ~priv:bob ~pub:(x25519 ~priv:alice ~pub:base)   

  (** Quickcheck generator for private keys *)
  let quickcheck_generator_private_key : private_key G.t = 
    (* Generate a byte array of length 32, where each byte corresponds to 
        an alphanumeric character *)
    let bytes = Bytes.init 32 ~f:(fun _ -> random_value G.char_alphanum) in 
    G.return @@ private_key_of_bytes bytes

  (** TODO: write a quickcheck generator for public keys *)
end  