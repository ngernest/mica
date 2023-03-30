open Base_quickcheck
open Stack
open Sexplib.Std

(** Testing harness for the [ListStack] module defined in [Stack.ml]*)
module StackTest = struct

  include ListStack

  (** Symbolic commands *)
  type cmd =
    | Push of char
    | Pop
    | Peek
    | Clear
    | Is_empty
    | Length 
    [@@deriving sexp_of, quickcheck]

  (** Type of the model's state *)
  type state = char list

  (** Type of the system under test *)
  type sut = char ListStack.t

  (** Generator of symbolic commands 
      (takes in a state argument for state-dependent command generation) *)
  let gen_cmd (st : state) : cmd Generator.t =
    Generator.union ((if Base.List.is_empty st
                then []
                else [Generator.return Pop;
                      Generator.return Peek])
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
    | Pop        -> Option.value (Base.List.tl st) ~default:[] 
    | Clear      -> []
    | Peek | Is_empty | Length -> st
  
  let init_sut   = ListStack.create
  let cleanup _  = ()

  (** Interprets the command [cmd] over the system under test
      Here, [st] refers to the model's state prior to executing [cmd] *)
  let run_cmd (cmd : cmd) (st : state) (sut : sut) : bool = 
    let module B = Base in 
    match cmd with
    | Push e   -> ListStack.(push e sut |> length) = B.List.length (e :: st)
    | Pop      -> (match ListStack.pop sut, B.List.hd st with 
                  | None, None -> true 
                  | _, None | None, _ -> false 
                  | Some e, Some e' -> e = e')
    | Peek      -> (match ListStack.peek sut, B.List.hd st with 
                  | None, None -> true 
                  | _, None | None, _ -> false 
                  | Some e, Some e' -> e = e')
    | Clear    -> ListStack.clear sut; true
    | Is_empty -> (ListStack.is_empty sut) = (st = [])
    | Length   -> (ListStack.length sut = B.List.length st)

  (** The [precond] function acts as a de-facto invariant function -- 
      this is invoked every time we generate [cmds] in the module StT below    
  *)
  let precond (cmd : cmd) (st : state) : bool = 
    match cmd with
    | Pop | Peek                          -> st <> []
    | Push _ | Clear | Is_empty | Length -> true

end

(** The TestHarness.Make functor returns a module containing
    functions that check for consistency and ensure that invariants are held *)
module StT = TestHarness.Make(StackTest);;

let%test_unit "consistency" = StT.consistency_test ~trials:1000;;
let%test_unit "agreement" = StT.agree_test ~trials:1000;;
