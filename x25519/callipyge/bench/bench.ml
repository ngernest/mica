let random_key len =
  let ic = open_in_bin "/dev/urandom" in
  let rs = really_input_string ic len in
  close_in ic; rs

let bench () =
  let secret = random_key 32 in
  let public = random_key 32 in

  let f () =
    let _ = Callipyge.shared
        ~secret:(Callipyge.secret_key_of_string secret)
        ~public:(Callipyge.public_key_of_string public) in
    () in
  let g () =
    let _ = Donna.shared ~secret ~public in
    () in

  Benchmark.throughputN 1 [ "callipyge", f, ()
                          ; "donna", g, () ]

let () =
  let open Benchmark.Tree in

  register @@ "benchmark" @>>> [ "ECDH" @> lazy (bench ()) ]

let () = Benchmark.Tree.run_global ()
