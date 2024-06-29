open Core.Quickcheck.Generator
open Core.Quickcheck.Generator.Let_syntax

let gen_int =
  let%bind x1 = small_non_negative_int and x2 = small_non_negative_int in
  return (x1, x2)

let gen_int' =
  bind (both small_non_negative_int small_non_negative_int) ~f:(fun (x1, x2) ->
      return (x1, x2) )

let gen_int'' =
  both small_non_negative_int small_non_negative_int
  >>= fun (x1, x2) -> return (x1, x2)

(*

    let%bind x1 = m1 and x2 = m2 in e === bind (both m1 m2) (fun (x1, x2) -> e)
    === both m1 m2 >>= fun (x1, x2) -> e *)
