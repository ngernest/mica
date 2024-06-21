open Core.Quickcheck.Generator
open Core.Quickcheck.Generator.Let_syntax 

let gen_int = 
  let%bind k = small_non_negative_int in 
  return k