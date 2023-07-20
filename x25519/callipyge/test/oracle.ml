external ecdh : bytes -> string -> string -> unit = "caml_curve25519_donna"
  [@@noalloc]

let base = String.init 32 (function 0 -> '\x09' | _ -> '\x00')

let public ~secret =
  let rs = Bytes.create 32 in
  ecdh rs secret base ; Bytes.unsafe_to_string rs

let shared ~secret ~public =
  let rs = Bytes.create 32 in
  ecdh rs secret public ; Bytes.unsafe_to_string rs
