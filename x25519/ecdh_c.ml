open ECDHIntf

(** Suppress "unused-value" compiler warnings *)
[@@@ocaml.warning "-32"]

module ECDH_C : ECDHIntf = struct 
  include Callipyge
  let sexp_of_public_key = sexp_of_public_key
  let sexp_of_private_key = sexp_of_private_key

  let base = public_key_of_string "9"  
  
  let public_key_of_string  = public_key_of_string
  let private_key_of_string = private_key_of_string

  let public_key_of_private_key private_key = public_of_secret private_key

  let scalar_mult private_key public_key = 
    let out = Array.make 32 0 in 
    ecdh_inplace ~out ~secret:private_key ~public:public_key;
    public_key_of_int_array out

  let x25519_ecdh private_key public_key = 
    shared ~secret:private_key ~public:public_key |> string_of_public_key 
  
end   