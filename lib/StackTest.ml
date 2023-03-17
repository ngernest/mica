open! Core
(* NB: need to open Core.Poly for polymorphic comparison *)
open Core.Poly
open Core.Quickcheck



(* NB: This module tests Core.Stack, although in reality we would be testing 
   Stack.ml *)
module StackTest = struct

  (** Definitions from the original module omitted here *)

  (** Symbolic commands *)
  type cmd =
    | Push of char
    | Pop
    | Top
    | Clear
    | Is_empty
    | Length 
    [@@deriving sexp_of, quickcheck]

  (** Type of the model's state *)
  type state = char list

  (** Type of the system under test *)
  type sut = char Stack.t

  (** Generator of symbolic commands 
      (takes in a state argument for state-dependent command generation) *)
  let gen_cmd (st : state) : cmd Generator.t =
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

  (** Given a command [cmd] and the current state [st], move 
      the model's state to the next state by interpreting [cmd] *)
  let next_state (cmd : cmd) (st : state) : state = 
    match cmd with
    | Push e     -> e::st
    | Pop        -> Option.value (List.tl st) ~default:[] 
    | Clear      -> []
    | Top | Is_empty | Length -> st
  
  let init_sut   = Stack.create
  let cleanup _  = ()

  (** Interprets the command [cmd] over the system under test
      Here, [st] refers to the model's state prior to executing [cmd] *)
  let run_cmd (cmd : cmd) (st : state) (sut : sut) : bool = 
    match cmd with
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

  (** The [precond] function acts as a de-facto invariant function -- this is invoked every time we generate [cmds] 
  in the module StT below    
  *)
  let precond (cmd : cmd) (st : state) : bool = 
    match cmd with
    | Pop | Top                          -> st <> []
    | Push _ | Clear | Is_empty | Length -> true

end

(** The TestHarness.Make functor returns a module containing
    functions that check for consistency and ensure that invariants are held *)
module StT = TestHarness.Make(StackTest);;

let%test_unit "consistency" = StT.consistency_test ~trials:1000;;
let%test_unit "agreement" = StT.agree_test ~trials:1000;;
