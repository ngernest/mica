(** Generated property-based testing code *)
open Base
open Base_quickcheck
open X25519_ecdh
open ECDHIntf
open Ecdh_ocaml
open Ecdh_c

(** Suppress "unused value" compiler warnings *)
[@@@ocaml.warning "-27-32-33-34"]

type expr =
  | String of string (* <---- NEW *)
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
    | ValPrivateKey of M.private_key
    | ValPublicKey of M.public_key
    | ValString of string
      [@@deriving sexp_of]

  let rec interp (expr : expr) : value = 
    match expr with
     | String s -> ValString s
     | Base -> ValPublicKey (M.base)
     | Public_key_of_string s ->
      ValPublicKey (M.public_key_of_string s)
     | Private_key_of_string s ->
      ValPrivateKey (M.private_key_of_string s)
     | String_of_public_key pk ->
        begin match interp pk with 
        | ValPublicKey pk' -> ValString (M.string_of_public_key pk')
        | _ -> failwith "impossible"
        end 
     | String_of_private_key pk ->
        begin match interp pk with 
        | ValPrivateKey pk' -> ValString (M.string_of_private_key pk')
        | _ -> failwith "impossible"
        end 
     | Public_key_of_private_key pk ->
        begin match interp pk with 
        | ValPrivateKey pk' -> ValPublicKey (M.public_key_of_private_key pk')
        | _ -> failwith "impossible"
        end
     | Scalar_mult(pk1, pk2) ->
        begin match interp pk1, interp pk2 with 
        | ValPrivateKey pk1', ValPublicKey pk2' -> 
            ValPublicKey (M.scalar_mult pk1' pk2')
        | _ -> failwith "impossible"
        end
     | X25519_ecdh(pk1, pk2) ->
        begin match interp pk1, interp pk2 with 
        | ValPrivateKey pk1', ValPublicKey pk2' -> 
            ValString (M.x25519_ecdh pk1' pk2')
        | _ -> failwith "impossible"
       end
end

let rec gen_expr (ty : ty) : expr Generator.t = 
  let module G = Generator in 
  let open G.Let_syntax in 
  let%bind k = G.size in 
  match ty, k with 
    | (Private_key, _) ->
      let private_key_of_string = 
        let%bind s = gen_hex_string in
        G.return @@ Private_key_of_string s
      in private_key_of_string
    | (Public_key, _) ->
      let public_key_of_string = 
        let%bind s = gen_hex_string in
        G.return @@ Public_key_of_string s in 
      let public_key_of_private_key = 
        let%bind pk = G.with_size ~size:(k / 2) (gen_expr Private_key) in
        G.return @@ Public_key_of_private_key pk in 
      let scalar_mult = 
        let%bind pk1 = G.with_size ~size:(k / 2) (gen_expr Private_key) in
        let%bind pk2 = G.with_size ~size:(k / 2) (gen_expr Public_key) in
        G.return @@ Scalar_mult(pk1, pk2)
      in G.union [
        public_key_of_string;
        public_key_of_private_key;
        scalar_mult
      ]
    | (String, _) ->
      let string_of_public_key = 
        let%bind pk = G.with_size ~size:(k / 2) (gen_expr Public_key) in
        G.return @@ String_of_public_key pk in 
      let string_of_private_key = 
        let%bind pk = G.with_size ~size:(k / 2) (gen_expr Private_key) in
        G.return @@ String_of_private_key pk in 
      let x25519_ecdh = 
        let%bind pk1 = G.with_size ~size:(k / 2) (gen_expr Private_key) in
        let%bind pk2 = G.with_size ~size:(k / 2) (gen_expr Public_key) in
        G.return @@ X25519_ecdh(pk1, pk2)
      in G.union [
        string_of_public_key;
        string_of_private_key;
        x25519_ecdh
      ]


module I1 = ExprToImpl(ECDH_OCaml)
module I2 = ExprToImpl(ECDH_C)

let displayError (e : expr) (v1 : I1.value) (v2 : I2.value) : string = 
  Printf.sprintf "e = %s, v1 = %s, v2 = %s\n"
    (Sexp.to_string @@ sexp_of_expr e)
    (Sexp.to_string @@ [%sexp_of: I1.value] v1)
    (Sexp.to_string @@ [%sexp_of: I2.value] v2)