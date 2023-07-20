external ecdh: bytes -> string -> string -> unit = "caml_curve25519_donna" [@@noalloc]

let shared ~secret ~public =
  let rs = Bytes.create 32 in
    ecdh rs secret public
  ; Bytes.unsafe_to_string rs
