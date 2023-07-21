(** Type of keys. *)
type public_key = int array [@@deriving sexp_of]

type private_key = int array [@@deriving sexp_of]

type shared_key = int array [@@deriving sexp_of]

(* val sexp_of_public_key : int64 array -> Sexplib0__Sexp.t
val sexp_of_private_key : int64 array -> Sexplib0__Sexp.t
val sexp_of_shared_key : int64 array -> Sexplib0__Sexp.t *)

val base : public_key
(** The base point 9. *)

val private_key_of_string : string -> private_key
(** [private_key_of_string v] is private_key of a 32-bytes [string] [v]. It makes
    a fresh allocated {!private_key}. *)

val private_key_of_int_array : int array -> private_key
(** [private_key_of_string v] is private_key of a 32-bytes [int array] [v]. It
    only verifies [v] (no allocation). *)

val public_key_of_string : string -> public_key
(** [public_key_of_string v] is public_key of 32-bytes [string] [v]. Null
    public_key ([String.make 32 '\x00']) is not allowed. It makes a fresh
    allocated {!public_key}. *)

val public_key_of_int_array : int array -> public_key
(** [public_key_of_int_array v] is public_key of 32-bytes [int array] [v]. It
    only verifies [v] (no allocation). Null public_key ([Array.make 32 0]) is
    not allowed. *)

val string_of_public_key : public_key -> string
(** [string_of_public_key k] makes a fresh allocated [string] of [k]. *)

val string_of_private_key : private_key -> string
val string_of_shared_key : shared_key -> string

val ecdh_inplace :
  out:int array -> secret:private_key -> public:public_key -> unit
(** [ecdh_inplace ~out ~secret ~public] computes the shared secret between
    private_key [secret] and public_key [public]. The result is stored in [out]. *)

val ecdh_base_inplace : out:int array -> secret:private_key -> unit
(** [ecdh_base_inplace ~out ~secret] is eqauivalent to {!ecdh} with the secret
    key [secret] and the base point {!base}, with the resulting public_key
    stored in [out]. *)

val public_of_secret : private_key -> public_key
(** [public_of_secret k] is public_key of [k]. It makes a fresh allocated
    public_key. *)

val shared : secret:private_key -> public:public_key -> shared_key
(** [shared ~secret ~public] computes the shared secret between private_key
    [secret] and public_key [public]. It makes a fresh allocated result. *)

val public_key_of_shared : shared_key -> public_key
(** [public_key_of_shared k] maps [k] to be a public_key. *)

val private_key_of_shared : shared_key -> private_key
(** [private_key_of_shared k] maps [k] to be a private_key. *)

val pp_public_key : public_key Fmt.t
(** [pp_public_key ppf v] prints public_key [v] on [ppf]. *)

val pp_shared_key : shared_key Fmt.t
(** [pp_shared_key ppf v] prints shared_key [v] on [ppf]. *)

val equal_public_key : public_key -> public_key -> bool
val equal_private_key : private_key -> private_key -> bool
val equal_shared_key : shared_key -> shared_key -> bool


