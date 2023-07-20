(** Type of keys. *)
type _ key = private int array

and public

and secret

and shared

val base : public key
(** The base point 9. *)

val secret_key_of_string : string -> secret key
(** [secret_key_of_string v] is secret key of a 32-bytes [string] [v]. It makes
    a fresh allocated {!secret key}. *)

val secret_key_of_int_array : int array -> secret key
(** [secret_key_of_string v] is secret key of a 32-bytes [int array] [v]. It
    only verifies [v] (no allocation). *)

val public_key_of_string : string -> public key
(** [public_key_of_string v] is public key of 32-bytes [string] [v]. Null
    public key ([String.make 32 '\x00']) is not allowed. It makes a fresh
    allocated {!public key}. *)

val public_key_of_int_array : int array -> public key
(** [public_key_of_int_array v] is public key of 32-bytes [int array] [v]. It
    only verifies [v] (no allocation). Null public key ([Array.make 32 0]) is
    not allowed. *)

val string_of_key : _ key -> string
(** [string_of_key k] makes a fresh allocated [string] of [k]. *)

val ecdh_inplace :
  out:int array -> secret:secret key -> public:public key -> unit
(** [ecdh_inplace ~out ~secret ~public] computes the shared secret between
    secret key [secret] and public key [public]. The result is stored in [out]. *)

val ecdh_base_inplace : out:int array -> secret:secret key -> unit
(** [ecdh_base_inplace ~out ~secret] is eqauivalent to {!ecdh} with the secret
    key [secret] and the base point {!base}, with the resulting public key
    stored in [out]. *)

val public_of_secret : secret key -> public key
(** [public_of_secret k] is public key of [k]. It makes a fresh allocated
    public key. *)

val shared : secret:secret key -> public:public key -> shared key
(** [shared ~secret ~public] computes the shared secret between secret key
    [secret] and public key [public]. It makes a fresh allocated result. *)

val public_key_of_shared : shared key -> public key
(** [public_key_of_shared k] maps [k] to be a public key. *)

val secret_key_of_shared : shared key -> secret key
(** [secret_key_of_shared k] maps [k] to be a secret key. *)

val pp_public_key : public key Fmt.t
(** [pp_public_key ppf v] prints public key [v] on [ppf]. *)

val pp_shared_key : shared key Fmt.t
(** [pp_shared_key ppf v] prints shared key [v] on [ppf]. *)

val equal_key : 'a key -> 'a key -> bool
(** [equal_key k1 k2] returns [true] iff [k1 = k2]. Otherwise, it returns
    [false]. *)
