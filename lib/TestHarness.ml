open! Core
open Core.Quickcheck

module type Spec =
sig
  type cmd
  (** The type of commands *)

  type state
  (** The type of the model's state *)

  type sut
  (** The type of the system under test *)

  val gen_cmd : state -> cmd Generator.t
  (** A command generator. Accepts a state parameter to enable state-dependent {!cmd} generation. *)

  val init_state : state
  (** The model's initial state. *)

  val next_state : cmd -> state -> state
  (** [next_state c s] expresses how interpreting the command [c] moves the
      model's internal state machine from the state [s] to the next state.
      Ideally a [next_state] function is pure. *)

  val init_sut : unit -> sut
  (** Initialize the system under test. *)

  val cleanup : sut -> unit
  (** Utility function to clean up the {!sut} after each test instance,
      e.g., for closing sockets, files, or resetting global parameters*)

  val precond : cmd -> state -> bool
  (** [precond c s] expresses preconditions for command [c] in terms of the model state [s].
      A [precond] function should be pure.
      [precond] is useful, e.g., to prevent the shrinker from breaking invariants when minimizing
      counterexamples. *)

  val run_cmd : cmd -> state -> sut -> bool
  (** [run_cmd c s i] should interpret the command [c] over the system under test (typically side-effecting).
      [s] is in this case the model's state prior to command execution.
      The returned Boolean value should indicate whether the interpretation went well
      and in case [c] returns a value: whether the returned value agrees with the model's result. *)
end

module Make(Spec : Spec) = struct

  (* gen_cmds : Spec.state -> int -> cSpec.md list Base_quickcheck.Generator.t *)
  let rec gen_cmds st fuel =
    let open Quickcheck.Generator in 
    if fuel = 0 then return []
    else Spec.gen_cmd st >>= fun cmd ->
	   (gen_cmds (Spec.next_state cmd st) (fuel - 1)) >>= fun cmds -> 
        return (cmd::cmds)
  (** A fueled command list generator.
      Accepts a state parameter to enable state-dependent [cmd] generation. *)


  let rec cmds_ok st cmds = 
    match cmds with
    | [] -> true
    | c::cs -> Spec.precond c st &&
      let s' = Spec.next_state c st in
        cmds_ok s' cs
  (** A precondition check (stops early due to short-circuit Boolean evaluation).
      Accepts the initial state [st] & the command sequnece [cmds] as parameters. *)

  (* arb_cmds : Spec.state -> Spec.cmd list Base_quickcheck.Generator.t *)
  let arb_cmds (st : Spec.state) = 
    let open Generator.Monad_infix in 
    Int.gen_incl 0 10 >>= fun size -> gen_cmds st size

  (* TODO: handle shrinking later *)
  (** A generator of command sequences. Accepts the initial state as parameter. *)

  let consistency_test ~trials =
    Quickcheck.test ~trials (arb_cmds Spec.init_state) 
      ~f:(fun cmds -> 
        [%test_result: bool] ~expect:true (cmds_ok Spec.init_state cmds))
  (** A consistency test that generates a number of [cmd] sequences and
      checks that all contained [cmd]s satisfy the precondition [precond]. 
      Accepts an optional [count] parameter and a test name as a labeled parameter [name]. *)

  let rec interp_agree s sut cs = match cs with
  | [] -> true
  | c::cs ->
    let b = Spec.run_cmd c s sut in
    let s' = Spec.next_state c s in
    b && interp_agree s' sut cs
  (** Checks agreement between the model and the system under test
      (stops early, thanks to short-circuit Boolean evaluation). *)

  (* agree_prop : Spec.cmd list -> bool *)
  let agree_prop : Spec.cmd list -> bool =
    fun cs ->
        assert (cmds_ok Spec.init_state cs);
        let sut = Spec.init_sut () in (* reset system's state *)
        let res = interp_agree Spec.init_state sut cs in
        let ()  = Spec.cleanup sut in
        res
        (* if res then Ok res 
        else Or_error.error "Model & SUT don't agree!" res Bool.sexp_of_t *)
  (** The agreement property: the command sequence [cs] yields the same observations
      when interpreted from the model's initial state and the [sut]'s initial state.
      Cleans up after itself by calling [Spec.cleanup] *)
    
  let agree_test ~trials =
    Quickcheck.test ~trials (arb_cmds Spec.init_state) 
      ~f:(fun cmds -> 
        [%test_result: bool] ~expect:true (agree_prop cmds))
  (** An actual agreement test (for convenience). Accepts an optional count parameter
      and a test name as a labeled parameter [name]. *)
end