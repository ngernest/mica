(** Generated property-based testing code *)
open Base
open Base_quickcheck
open X25519_ecdh
open ECDHIntf
open ECDH_OCaml
open ECDH_C

(** Suppress "unused value" compiler warnings *)
[@@@ocaml.warning "-27-32-33-34"]

type expr =
  | Base
  | Public_key_of_string of string
  | Private_key_of_string of string
  | String_of_public_key of expr
  | String_of_private_key of expr
  | Public_key_of_private_key of expr
  | Scalar_mult of expr * expr
  | X25519_ecdh of expr * expr
    [@@deriving sexp_of]

type ty =
  Private_key | Public_key | String
    [@@deriving sexp_of]

module ExprToImpl (M : ECDHIntf) = struct 
  include M

  type value = 
    | ValPrivate_key of M.private_key
    | ValPublic_key of M.public_key
    | ValString of string
      [@@deriving sexp_of]

  let rec interp (expr : expr) : value = 
    match expr with
     | Base -> ValPublic_key (M.base)
     | Public_key_of_string s ->
      ValPublic_key (M.public_key_of_string s)
     | Private_key_of_string s ->
      ValPrivate_key (M.private_key_of_string s)
     | String_of_public_key pk ->
        begin match interp pk with 
        | ValPublic_key pk' -> ValString (M.string_of_public_key pk')
        | _ -> failwith "impossible"
        end 
     | String_of_private_key pk ->
        begin match interp pk with 
        | ValPrivate_key pk' -> ValString (M.string_of_private_key pk')
        | _ -> failwith "impossible"
        end 
     | Public_key_of_private_key pk ->
        begin match interp pk with 
        | ValPrivate_key pk' -> ValPublic_key (M.public_key_of_private_key pk')
        | _ -> failwith "impossible"
        end
     | Scalar_mult(pk1, pk2) ->
        begin match interp pk1, interp pk2 with 
        | ValPrivate_key pk1', ValPublic_key pk2' -> 
            ValPublic_key (M.scalar_mult pk1' pk2')
        | _ -> failwith "impossible"
        end
     | X25519_ecdh(pk1, pk2) ->
        begin match interp pk1, interp pk2 with 
        | ValPrivate_key pk1', ValPublic_key pk2' -> 
            ValString (M.x25519_ecdh pk1' pk2')
        | _ -> failwith "impossible"
       end

  let rec gen_expr (ty : ty) : expr Generator.t = 
    let module G = Generator in 
    let open G.Let_syntax in 
    let%bind k = G.size in 
    match ty, k with 
      | (Private_key, _) ->
        let private_key_of_string = 
          let%bind s = G.string_non_empty in
          G.return @@ Private_key_of_string s
        in private_key_of_string
      | (Public_key, _) ->
        let public_key_of_string = 
          let%bind s = G.string_non_empty in
          G.return @@ Public_key_of_string s in 
        let public_key_of_private_key = 
          let%bind pk = [%quickcheck.generator: M.private_key] in
          G.return @@ Public_key_of_private_key pk in 
        let scalar_mult = 
          let%bind pk1 = [%quickcheck.generator: private_key] in
          let%bind pk2 = [%quickcheck.generator: public_key] in
          G.return @@ Scalar_mult(pk1, pk2)
        in G.union [
          public_key_of_string;
          public_key_of_private_key;
          scalar_mult
        ]
      | (String, _) ->
        let string_of_public_key = 
          let%bind pk = [%quickcheck.generator: public_key] in
          G.return @@ String_of_public_key pk in 
        let string_of_private_key = 
          let%bind pk = [%quickcheck.generator: private_key] in
          G.return @@ String_of_private_key pk in 
        let x25519_ecdh = 
          let%bind pk1 = [%quickcheck.generator: private_key] in
          let%bind pk2 = [%quickcheck.generator: public_key] in
          G.return @@ X25519_ecdh(pk1, pk2)
        in G.union [
          string_of_public_key;
          string_of_private_key;
          x25519_ecdh
        ]
end



module I1 = ExprToImpl(ECDH_OCaml)
module I2 = ExprToImpl(ECDH_C)

let displayError (e : expr) (v1 : I1.value) (v2 : I2.value) : string = 
  Printf.sprintf "e = %s, v1 = %s, v2 = %s\n"
    (Sexp.to_string @@ sexp_of_expr e)
    (Sexp.to_string @@ [%sexp_of: I1.value] v1)
    (Sexp.to_string @@ [%sexp_of: I2.value] v2)