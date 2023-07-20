module Array = Ma

let basev = Array.init 32 (function 0 -> 9 | _ -> 0)
let minusp = Array.init 32 (function 0 -> 19 | 31 -> 128 | _ -> 0)

let pp ppf arr =
  for i = 0 to Array.length arr - 1 do
    Fmt.pf ppf "%02X" arr.(i)
  done

let add outv outv_offset a a_offset b b_offset =
  let u = ref 0 in
  for j = 0 to 30 do
    u := !u + a.(a_offset + j) + b.(b_offset + j) ;
    outv.(outv_offset + j) <- !u land 255 ;
    u := !u lsr 8
  done ;
  u := !u + a.(a_offset + 31) + b.(b_offset + 31) ;
  outv.(outv_offset + 31) <- !u

let sub outv outv_offset a a_offset b b_offset =
  let u = ref 218 in
  for j = 0 to 30 do
    u := !u + a.(a_offset + j) + 65280 - b.(b_offset + j) ;
    outv.(outv_offset + j) <- !u land 255 ;
    u := !u lsr 8
  done ;
  u := !u + a.(a_offset + 31) - b.(b_offset + 31) ;
  outv.(outv_offset + 31) <- !u

let squeeze a a_offset =
  let u = ref 0 in
  for j = 0 to 30 do
    u := !u + a.(a_offset + j) ;
    a.(a_offset + j) <- !u land 255 ;
    u := !u lsr 8
  done ;
  u := !u + a.(a_offset + 31) ;
  a.(a_offset + 31) <- !u land 127 ;
  u := 19 * (!u lsr 7) ;
  for j = 0 to 30 do
    u := !u + a.(a_offset + j) ;
    a.(a_offset + j) <- !u land 255 ;
    u := !u lsr 8
  done ;
  u := !u + a.(a_offset + 31) ;
  a.(a_offset + 31) <- !u

let freeze a a_offset =
  let a_orig = Array.init 32 (fun j -> a.(a_offset + j)) in
  add a 0 a 0 minusp 0 ;
  let negative = -((a.(a_offset + 31) lsr 7) land 1) in
  for j = 0 to 31 do
    a.(a_offset + j)
    <- a.(a_offset + j) lxor (negative land (a_orig.(j) lxor a.(a_offset + j)))
  done

let mult outv outv_offset a a_offset b b_offset =
  for i = 0 to 31 do
    let u = ref 0 in
    for j = 0 to i do
      u := !u + (a.(a_offset + j) * b.(b_offset + i - j))
    done ;
    for j = i + 1 to 31 do
      u := !u + (38 * a.(a_offset + j) * b.(b_offset + i + 32 - j))
    done ;
    outv.(outv_offset + i) <- !u
  done ;
  squeeze outv outv_offset

let mult21665 outv a =
  let u = ref 0 in
  for j = 0 to 30 do
    u := !u + (121665 * a.(j)) ;
    outv.(j) <- !u land 255 ;
    u := !u lsr 8
  done ;
  u := !u + (121665 * a.(31)) ;
  outv.(31) <- !u land 127 ;
  u := 19 * (!u lsr 7) ;
  for j = 0 to 30 do
    u := !u + outv.(j) ;
    outv.(j) <- !u land 255 ;
    u := !u lsr 8
  done ;
  u := !u + outv.(31) ;
  outv.(31) <- !u

let square outv outv_offset a a_offset =
  for i = 0 to 31 do
    let u = ref 0 in
    let j = ref 0 in
    while !j < i - !j do
      u := !u + (a.(a_offset + !j) * a.(a_offset + i - !j)) ;
      incr j
    done ;
    j := i + 1 ;
    while !j < i + 32 - !j do
      u := !u + (38 * a.(a_offset + !j) * a.(a_offset + i + 32 - !j)) ;
      incr j
    done ;
    u := !u * 2 ;
    if i land 1 = 0
    then (
      u := !u + (a.(a_offset + (i / 2)) * a.(a_offset + (i / 2))) ;
      u := !u + (38 * a.(a_offset + (i / 2) + 16) * a.(a_offset + (i / 2) + 16)) ) ;
    outv.(outv_offset + i) <- !u
  done ;
  squeeze outv outv_offset

let select p q r s b =
  let bminus1 = b - 1 in
  for j = 0 to 63 do
    let t = bminus1 land (r.(j) lxor s.(j)) in
    p.(j) <- s.(j) lxor t ;
    q.(j) <- r.(j) lxor t
  done

let main_loop work e =
  let xzm1 = Array.make 64 0 in
  let xzm = Array.make 64 0 in
  let xzmb = Array.make 64 0 in
  let xzm1b = Array.make 64 0 in
  let xznb = Array.make 64 0 in
  let xzn1b = Array.make 64 0 in
  let a0 = Array.make 64 0 in
  let a1 = Array.make 64 0 in
  let b0 = Array.make 64 0 in
  let b1 = Array.make 64 0 in
  let c1 = Array.make 64 0 in
  let r = Array.make 32 0 in
  let s = Array.make 32 0 in
  let t = Array.make 32 0 in
  let u = Array.make 32 0 in
  for j = 0 to 31 do
    xzm1.(j) <- work.(j)
  done ;
  xzm1.(32) <- 1 ;
  xzm.(0) <- 1 ;
  for pos = 254 downto 0 do
    let b = (e.(pos / 8) land 0xFF) lsr (pos land 7) in
    let b = b land 1 in
    select xzmb xzm1b xzm xzm1 b ;
    add a0 0 xzmb 0 xzmb 32 ;
    sub a0 32 xzmb 0 xzmb 32 ;
    add a1 0 xzm1b 0 xzm1b 32 ;
    sub a1 32 xzm1b 0 xzm1b 32 ;
    square b0 0 a0 0 ;
    square b0 32 a0 32 ;
    mult b1 0 a1 0 a0 32 ;
    mult b1 32 a1 32 a0 0 ;
    add c1 0 b1 0 b1 32 ;
    sub c1 32 b1 0 b1 32 ;
    square r 0 c1 32 ;
    sub s 0 b0 0 b0 32 ;
    mult21665 t s ;
    add u 0 t 0 b0 0 ;
    mult xznb 0 b0 0 b0 32 ;
    mult xznb 32 s 0 u 0 ;
    square xzn1b 0 c1 0 ;
    mult xzn1b 32 r 0 work 0 ;
    select xzm xzm1 xznb xzn1b b
  done ;
  for j = 0 to 63 do
    work.(j) <- xzm.(j)
  done

let recip outv outv_offset z z_offset =
  let z2 = Array.make 32 0 in
  let z9 = Array.make 32 0 in
  let z11 = Array.make 32 0 in
  let z2_5_0 = Array.make 32 0 in
  let z2_10_0 = Array.make 32 0 in
  let z2_20_0 = Array.make 32 0 in
  let z2_50_0 = Array.make 32 0 in
  let z2_100_0 = Array.make 32 0 in
  let t0 = Array.make 32 0 in
  let t1 = Array.make 32 0 in
  square z2 0 z z_offset ;
  (* 2 *)
  square t1 0 z2 0 ;
  (* 4 *)
  square t0 0 t1 0 ;
  (* 8 *)
  mult z9 0 t0 0 z z_offset ;
  (* 9 *)
  mult z11 0 z9 0 z2 0 ;
  (* 11 *)
  square t0 0 z11 0 ;
  (* 22 *)
  mult z2_5_0 0 t0 0 z9 0 ;
  (* 2^5 - 2^0 = 31 *)
  square t0 0 z2_5_0 0 ;
  (* 2^6 - 2^1 *)
  square t1 0 t0 0 ;
  (* 2^7 - 2^2 *)
  square t0 0 t1 0 ;
  (* 2^8 - 2^3 *)
  square t1 0 t0 0 ;
  (* 2^9 - 2^4 *)
  square t0 0 t1 0 ;
  (* 2^10 - 2^5 *)
  mult z2_10_0 0 t0 0 z2_5_0 0 ;
  (* 2^10 - 2^0 *)
  square t0 0 z2_10_0 0 ;
  (* 2^11 - 2^1 *)
  square t1 0 t0 0 ;
  (* 2^12 - 2^2 *)
  
  (* 2^20 - 2^10 *)
  for _ = 1 to 4 do
    square t0 0 t1 0 ; square t1 0 t0 0
  done ;
  mult z2_20_0 0 t1 0 z2_10_0 0 ;
  (* 2^20 - 2^0 *)
  square t0 0 z2_20_0 0 ;
  (* 2^21 - 2^1 *)
  square t1 0 t0 0 ;
  (* 2^22 - 2^2 *)
  
  (* 2^40 - 2^40 *)
  for _ = 1 to 9 do
    square t0 0 t1 0 ; square t1 0 t0 0
  done ;
  mult t0 0 t1 0 z2_20_0 0 ;
  (* 2^40 - 2^0 *)
  square t1 0 t0 0 ;
  (* 2^41 - 2^1 *)
  square t0 0 t1 0 ;
  (* 2^42 - 2^2 *)
  
  (* 2^50 - 2^10 *)
  for _ = 1 to 4 do
    square t1 0 t0 0 ; square t0 0 t1 0
  done ;
  mult z2_50_0 0 t0 0 z2_10_0 0 ;
  (* 2^50 - 2^0 *)
  square t0 0 z2_50_0 0 ;
  (* 2^51 - 2^1 *)
  square t1 0 t0 0 ;
  (* 2^52 - 2^2 *)
  
  (* 2^100 - 2^50 *)
  for _ = 1 to 24 do
    square t0 0 t1 0 ; square t1 0 t0 0
  done ;
  mult z2_100_0 0 t1 0 z2_50_0 0 ;
  (* 2^100 - 2^0 *)
  square t1 0 z2_100_0 0 ;
  (* 2^101 - 2^1 *)
  square t0 0 t1 0 ;
  (* 2^102 - 2^2 *)
  
  (* 2^200 - 2^100 *)
  for _ = 1 to 49 do
    square t1 0 t0 0 ; square t0 0 t1 0
  done ;
  mult t1 0 t0 0 z2_100_0 0 ;
  (* 2^200 - 2^0 *)
  square t0 0 t1 0 ;
  (* 2^201 - 2^1 *)
  square t1 0 t0 0 ;
  (* 2^202 - 2^2 *)
  
  (* 2^250 - 2^50 *)
  for _ = 1 to 24 do
    square t0 0 t1 0 ; square t1 0 t0 0
  done ;
  mult t0 0 t1 0 z2_50_0 0 ;
  (* 2^250 - 2^0 *)
  square t1 0 t0 0 ;
  (* 2^251 - 2^1 *)
  square t0 0 t1 0 ;
  (* 2^252 - 2^2 *)
  square t1 0 t0 0 ;
  (* 2^253 - 2^3 *)
  square t0 0 t1 0 ;
  (* 2^254 - 2^4 *)
  square t1 0 t0 0 ;
  (* 2^255 - 2^5 *)
  mult outv outv_offset t1 0 z11 0

(* 2^255 - 21 *)

let curve25519 q n p =
  let work = Array.make 96 0 in
  let e = Array.make 32 0 in
  for i = 0 to 31 do
    e.(i) <- n.(i)
  done ;
  e.(0) <- e.(0) land 248 ;
  e.(31) <- e.(31) land 127 ;
  e.(31) <- e.(31) lor 64 ;
  for i = 0 to 31 do
    work.(i) <- p.(i) land 0xFF
  done ;
  main_loop work e ;
  recip work 32 work 32 ;
  mult work 64 work 0 work 32 ;
  freeze work 64 ;
  for i = 0 to 31 do
    q.(i) <- work.(64 + i)
  done ;
  ()

let curve25519_base q n =
  let basevp = basev in
  curve25519 q n basevp

type _ key = int array
type public
type secret
type shared

external identity : 'a -> 'a = "%identity"

let ( <.> ) f g x = f (g x)
let base = basev

let secret_key_of_string : string -> secret key =
 fun x ->
  if String.length x <> 32
  then Fmt.invalid_arg "secret_key_of_string: invalid key" ;
  Array.init 32 (Char.code <.> String.get x)

let secret_key_of_int_array : int array -> secret key =
 fun x ->
  if Array.length x <> 32 || Array.exists (fun x -> x > 0xFF) x
  then
    Fmt.invalid_arg "public_key_of_int_array: key should consist of 32 bytes" ;
  identity x

let null = String.make 32 '\x00'

let public_key_of_string : string -> public key =
 fun x ->
  if String.length x <> 32
  then Fmt.invalid_arg "public_key_of_string: key should consist of 32 bytes" ;
  if String.equal x null
  then Fmt.invalid_arg "public_key_of_string: null public key" ;
  Array.init 32 (Char.code <.> String.get x)

let public_key_of_int_array : int array -> public key =
 fun x ->
  if Array.length x <> 32 || Array.exists (fun x -> x > 0xFF) x
  then
    Fmt.invalid_arg "public_key_of_int_array: key should consist of 32 bytes" ;
  if Array.for_all (( = ) 0) x
  then Fmt.invalid_arg "public_key_of_int_array: null public key" ;
  identity x

let string_of_key : _ key -> string =
 fun x ->
  (* assert (Array.length x = 32); *)
  String.init 32 (Char.chr <.> Array.get x)

let ecdh_base_inplace : out:int array -> secret:secret key -> unit =
 fun ~out ~secret -> curve25519_base out secret

let ecdh_inplace :
    out:int array -> secret:secret key -> public:public key -> unit =
 fun ~out ~secret ~public -> curve25519 out secret public

let public_of_secret : secret key -> public key =
 fun secret ->
  let out = Array.make 32 0 in
  ecdh_base_inplace ~out ~secret ;
  out

let shared : secret:secret key -> public:public key -> public key =
 fun ~secret ~public ->
  let out = Array.make 32 0 in
  ecdh_inplace ~out ~secret ~public ;
  out

let[@noalloc] [@inline] public_key_of_shared x = identity x
let[@noalloc] [@inline] secret_key_of_shared x = identity x
let pp_public_key : public key Fmt.t = fun ppf key -> pp ppf key
let pp_shared_key : shared key Fmt.t = fun ppf key -> pp ppf key

let equal_key : _ key -> _ key -> bool =
 fun a b ->
  (* assert (Array.length a = 32 && Array.length b = 32); *)
  Eqaf.equal (string_of_key a) (string_of_key b)
