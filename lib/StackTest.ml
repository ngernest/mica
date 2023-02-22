open! Core
open Core.Poly
open Core.Quickcheck

(* NB: need to open Core.Poly for polymorphic comparison *)

module StConf = struct
  type cmd =
    | Push of char
    | Pop
    | Top
    | Clear
    | Is_empty
    | Length 
    [@@deriving sexp_of, quickcheck]

  type state = char list
  type sut = char Stack.t

  (*  gen_cmd : state -> cmd Gen.t *)
  let gen_cmd (st : state) =
    Generator.union ((if List.is_empty st
                then []
                else [Generator.return Pop;
                      Generator.return Top])
               @
               [Generator.map ~f:(fun c -> Push c) Generator.char;
                Generator.return Clear;
                Generator.return Is_empty;
                Generator.return Length])

  let init_state = []
  let next_state cmd st = match cmd with
    | Push e     -> e::st
    | Pop        -> Option.value (List.tl st) ~default:[] 
    | Clear      -> []
    | Top | Is_empty | Length -> st
  
  let init_sut   = Stack.create
  let cleanup _  = ()


  let run_cmd (cmd : cmd) (st : state) (sut : sut) = match cmd with
    | Push e   -> Stack.push sut e; true
    | Pop      -> (match Stack.pop sut, List.hd st with 
                  | None, None -> true 
                  | _, None | None, _ -> false 
                  | Some e, Some e' -> e = e')
    | Top      -> (match Stack.top sut, List.hd st with 
                  | None, None -> true 
                  | _, None | None, _ -> false 
                  | Some e, Some e' -> e = e')
    | Clear    -> Stack.clear sut; true
    | Is_empty -> (Stack.is_empty sut) = (st = [])
    | Length   -> (Stack.length sut = List.length st)

  let precond (cmd : cmd) (st : state) = 
    match cmd with
    | Pop | Top                          -> st <> []
    | Push _ | Clear | Is_empty | Length -> true

end

module StT = TestHarness.Make(StConf);;

let%test_unit "consistency" = StT.consistency_test ~trials:1000;;
let%test_unit "agreement" = StT.agree_test ~trials:1000;;
