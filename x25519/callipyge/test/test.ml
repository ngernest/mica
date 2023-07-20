let pp_base ppf arr =
  for i = 0 to Array.length arr - 1 do
    Fmt.pf ppf "%02X" arr.(i)
  done

type public_key = Callipyge.public Callipyge.key
type secret_key = Callipyge.secret Callipyge.key

let pp_secret ppf (arr : secret_key) = pp_base ppf (arr :> int array)

let doit ek (e : secret_key) (k : public_key) =
  Fmt.pr "%a %a " pp_secret e Callipyge.pp_public_key k ;
  Callipyge.ecdh_inplace ~out:ek ~secret:e ~public:k ;
  Fmt.pr "%a\n%!" pp_base ek

let ecdh (e1 : secret_key) (e2 : secret_key) (k : public_key) e1k e2k =
  let equal e1e2k e2e1k =
    let result =
      assert (Array.length e1e2k = 32 && Array.length e2e1k = 32) ;
      let rt = ref 0 in
      for i = 0 to 31 do
        rt := !rt lor (e1e2k.(i) lxor e2e1k.(i))
      done ;
      !rt = 0
    in
    Fmt.epr "%a equal %a: %b.\n%!" pp_base e1e2k pp_base e2e1k result ;
    if result
    then (
      Array.iteri
        (fun i x -> (e1 :> int array).(i) <- x lxor (e2k :> int array).(i))
        (e1 :> int array) ;
      Array.iteri
        (fun i x -> (e2 :> int array).(i) <- x lxor (e1k :> int array).(i))
        (e2 :> int array) ;
      Array.iteri
        (fun i x -> (k :> int array).(i) <- x lxor e1e2k.(i))
        (k :> int array) ;
      result )
    else result
  in
  let pp = pp_base in
  Alcotest.testable pp equal

let step (e1 : secret_key) (e2 : secret_key) (k : public_key) =
  let ecdh = ecdh e1 e2 k in
  ( "ecdh(e2, ecdh(e1, k)) = ecdh(e1, ecdh(e2, k))"
  , `Quick
  , fun () ->
      let e1k = Array.make 32 0 in
      let e2k = Array.make 32 0 in
      let e1e2k = Array.make 32 0 in
      let e2e1k = Array.make 32 0 in
      let () = doit e1k e1 k in
      let () = doit e2e1k e2 (Callipyge.public_key_of_int_array e1k) in
      let () = doit e2k e2 k in
      let () = doit e1e2k e1 (Callipyge.public_key_of_int_array e2k) in
      Alcotest.(check (ecdh e1k e2k)) "equal" e1e2k e2e1k )

let e1 =
  "\003\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"

let e2 =
  "\005\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"

let k =
  "\009\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"

let tests n =
  let e1 = Callipyge.secret_key_of_string e1 in
  let e2 = Callipyge.secret_key_of_string e2 in
  let k = Callipyge.public_key_of_string k in
  let step = step e1 e2 k in
  let rec go acc = function 0 -> acc | n -> go (step :: acc) (pred n) in
  go [] n

let public = Alcotest.testable Callipyge.pp_public_key Callipyge.equal_key

let shared =
  let equal a b =
    Fmt.pr "a: %a.\n%!" Callipyge.pp_shared_key a ;
    Fmt.pr "b: %a.\n%!" Callipyge.pp_shared_key b ;
    Callipyge.equal_key a b
  in
  Alcotest.testable Callipyge.pp_shared_key equal

let pp_key ppf x =
  for i = 0 to String.length x - 1 do
    Fmt.pf ppf "%02X" (Char.code (String.unsafe_get x i))
  done

let key =
  let equal a b =
    Fmt.pr "a: %a.\n%!" pp_key a ;
    Fmt.pr "b: %a.\n%!" pp_key b ;
    String.equal a b
  in
  Alcotest.testable pp_key equal

let nacl =
  let p_a =
    Callipyge.public_key_of_string
      "\x85\x20\xf0\x09\x89\x30\xa7\x54\x74\x8b\x7d\xdc\xb4\x3e\xf7\x5a\x0d\xbf\x3a\x0d\x26\x38\x1a\xf4\xeb\xa4\xa9\x8e\xaa\x9b\x4e\x6a"
  in
  let s_a =
    Callipyge.secret_key_of_string
      "\x77\x07\x6d\x0a\x73\x18\xa5\x7d\x3c\x16\xc1\x72\x51\xb2\x66\x45\xdf\x4c\x2f\x87\xeb\xc0\x99\x2a\xb1\x77\xfb\xa5\x1d\xb9\x2c\x2a"
  in
  let p_b =
    Callipyge.public_key_of_string
      "\xde\x9e\xdb\x7d\x7b\x7d\xc1\xb4\xd3\x5b\x61\xc2\xec\xe4\x35\x37\x3f\x83\x43\xc8\x5b\x78\x67\x4d\xad\xfc\x7e\x14\x6f\x88\x2b\x4f"
  in
  let s_b =
    Callipyge.secret_key_of_string
      "\x5d\xab\x08\x7e\x62\x4a\x8a\x4b\x79\xe1\x7f\x8b\x83\x80\x0e\xe6\x6f\x3b\xb1\x29\x26\x18\xb6\xfd\x1c\x2f\x8b\x27\xff\x88\xe0\xeb"
  in
  let p_alice () =
    Alcotest.(check public) "equal" (Callipyge.public_of_secret s_a) p_a
  in
  let p_bob () =
    Alcotest.(check public) "equal" (Callipyge.public_of_secret s_b) p_b
  in
  let p_shared () =
    let s_ab = Callipyge.shared ~secret:s_a ~public:p_b in
    let s_ba = Callipyge.shared ~secret:s_b ~public:p_a in
    Alcotest.(check shared) "equal" s_ab s_ba
  in
  ["alice", `Quick, p_alice; "bob", `Quick, p_bob; "shared", `Quick, p_shared]

let random_key () =
  let ic = open_in_bin "/dev/urandom" in
  let rs = really_input_string ic 32 in
  close_in ic ; rs

let list_init f n =
  let rec go acc = function
    | 0 -> List.rev acc
    | n -> go (f () :: acc) (pred n)
  in
  go [] n

let oracle =
  let test () =
    let donna_s_a = random_key () in
    let donna_s_b = random_key () in
    let donna_p_a = Oracle.public ~secret:donna_s_a in
    let donna_p_b = Oracle.public ~secret:donna_s_b in
    let donna_s_ab = Oracle.shared ~secret:donna_s_a ~public:donna_p_b in
    let donna_s_ba = Oracle.shared ~secret:donna_s_b ~public:donna_p_a in
    let callipyge_s_a = Callipyge.secret_key_of_string donna_s_a in
    let callipyge_s_b = Callipyge.secret_key_of_string donna_s_b in
    let callipyge_p_a = Callipyge.public_of_secret callipyge_s_a in
    let callipyge_p_b = Callipyge.public_of_secret callipyge_s_b in
    let callipyge_s_ab =
      Callipyge.shared ~secret:callipyge_s_a ~public:callipyge_p_b
    in
    let callipyge_s_ba =
      Callipyge.shared ~secret:callipyge_s_b ~public:callipyge_p_a
    in
    Fmt.pr "a: %a.\n%!" pp_base (callipyge_s_a :> int array) ;
    Fmt.pr "a: %a.\n%!" pp_key donna_s_a ;
    Fmt.pr "b: %a.\n%!" pp_base (callipyge_s_b :> int array) ;
    Fmt.pr "b: %a.\n%!" pp_key donna_s_b ;
    Alcotest.(check shared)
      "equal shared (callipyge)" callipyge_s_ab callipyge_s_ba ;
    Alcotest.(check key) "equal shared (oracle)" donna_s_ab donna_s_ba ;
    Alcotest.(check key)
      "equal public alice"
      (Callipyge.string_of_key callipyge_p_a)
      donna_p_a ;
    Alcotest.(check key)
      "equal public bob"
      (Callipyge.string_of_key callipyge_p_b)
      donna_p_b
  in
  list_init (fun () -> "oracle", `Quick, test) 64

let () =
  Alcotest.run "ECDH"
    [ "5 steps", tests 5
    ; "10 steps", tests 10
    ; "20 steps", tests 20
    ; "40 steps", tests 40
    ; "80 steps", tests 80
    ; "160 steps", tests 160
    ; "nacl", nacl
    ; "oracle", oracle ]
