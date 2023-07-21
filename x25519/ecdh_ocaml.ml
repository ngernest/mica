open Rfc7748
open ECDHIntf

(** Suppress "unused-value" compiler warnings *)
[@@@ocaml.warning "-32"]

module ECDH_OCaml : ECDHIntf = struct 
  include X25519
  
  let scalar_mult = scale 

  let x25519_ecdh (priv_k : private_key) (pub_k : public_key) : string = 
    scalar_mult priv_k pub_k |> string_of_public_key

  let shared_secret_alice (alice : string) (bob : string) (base : string) : string = 
    x25519 ~priv:alice ~pub:(x25519 ~priv:bob ~pub:base)

  let shared_secret_bob (bob : string) (alice : string) (base : string) : string = 
    x25519 ~priv:bob ~pub:(x25519 ~priv:alice ~pub:base)   
end  