open! Core
open Core.Quickcheck

module type SpecBase = sig
  type cmd
  (** The type of commands *)

  type state
  (** The type of the model's state *)

  val gen_cmd : state -> cmd Generator.t
  (** A command generator. Accepts a state parameter to enable state-dependent {!cmd} generation. *)

  val init_state : state
  (** The model's initial state. *)

  val next_state : cmd -> state -> state
  (** [next_state c s] expresses how interpreting the command [c] moves the
      model's internal state machine from the state [s] to the next state.
      Ideally a [next_state] function is pure. *)

  val precond : cmd -> state -> bool
  (** [precond c s] expresses preconditions for command [c] in terms of the model state [s].
      A [precond] function should be pure.
      [precond] is useful, e.g., to prevent the shrinker from breaking invariants when minimizing
      counterexamples. *)
end 


module type Spec1 = sig
  include SpecBase

  type sut
  (** The type of the system under test *)

  val init_sut : unit -> sut
  (** Initialize the system under test. *)

  val cleanup : sut -> unit
  (** Utility function to clean up the {!sut} after each test instance,
      e.g., for closing sockets, files, or resetting global parameters*)

  val run_cmd : cmd -> state -> sut -> bool
  (** [run_cmd c s i] should interpret the command [c] over the system under test (typically side-effecting).
      [s] is in this case the model's state prior to command execution.
      The returned Boolean value should indicate whether the interpretation went well
      and in case [c] returns a value: whether the returned value agrees with the model's result. *)
end

(** Extension of [SpecBase] for comparing two modules that implement the same interface *)
module type Spec2 = sig 
  include SpecBase
  
  (** Types of the two systems under tests (SUTs) *)
  type sutA 
  type sutB
  
  (** Initialization functions for the two SUTs *)
  val init_sutA : unit -> sutA 
  val init_sutB : unit -> sutB 

  (** Cleanup functions for the two SUTs *)
  val cleanupA : sutA -> unit 
  val cleanupB : sutB -> unit

  (** [compare_cmd cmd st sutA sutB] interprets the 
      symbolic command [cmd] over the two SUTs,
      and checks whether the result of interpreting [cmd] over [sutA] 
      & [sutB] agree with each other. 
      Here, [st] refers to the model's state prior to executing [cmd]. *)
  val compare_cmd : cmd -> state -> sutA -> sutB -> bool   
end 


module type HarnessBase = sig 
  type state 
  type cmd 

  val gen_cmds : state -> int -> cmd list Base_quickcheck.Generator.t
  (** A fueled command list generator.
    Accepts a state parameter to enable state-dependent [cmd] generation. *)

  val cmds_ok : state -> cmd list -> bool
  (** A precondition checker (stops early, thanks to short-circuit Boolean evaluation).
      Accepts the initial state and the command sequence as parameters.  *)

  val arb_cmds : state -> cmd list Base_quickcheck.Generator.t
  (** A generator of command sequences. Accepts the initial state as parameter. *)

  val consistency_test : trials:int -> unit
  (** A consistency test that generates a number of [cmd] sequences and
      checks that all contained [cmd]s satisfy the precondition [precond].
      Accepts a labeled parameters [trials], which is the no. of trials *)
end 


(** Extension of [HarnessBase], specialized for testing one module *)
module type Harness1 = sig 
  include HarnessBase 

  type sut

  val interp_agree : state -> sut -> cmd list -> bool
  (** Checks agreement between the model and the system under test
      (stops early, thanks to short-circuit Boolean evaluation). *)

  val agree_prop : cmd list -> bool
  (** The agreement property: the command sequence [cs] yields the same observations
    when interpreted from the model's initial state and the [sut]'s initial state.
    Cleans up after itself by calling relevant functions in other modules. *)   
end 

(** Extension of [HarnessBase], specialized for comparing two modules *)
module type Harness2 = sig 
  include HarnessBase 

  (** Types of the two systems under tests (SUTs) *)
  type sutA 
  type sutB

  val interp_agree : state -> sutA -> sutB -> cmd list -> bool
  (** Checks agreement between the model and the two SUTs *)

end

(** Given a PBT specification, creates a test harness that tests if a module 
    implements its signature properly *)
module Make1 (Spec : Spec1) : 
  (Harness1 with type state := Spec.state and 
  type cmd := Spec.cmd and type sut := Spec.sut) = struct

  open Spec

  (** A fueled command list generator.
    * Accepts a state parameter to enable state-dependent [cmd] generation. *)
  let rec gen_cmds (st : state) (fuel : int) : cmd list Generator.t =
    let open Quickcheck.Generator in 
    if fuel = 0 then return []
    else Spec.gen_cmd st >>= fun cmd ->
      (gen_cmds (Spec.next_state cmd st) (fuel - 1)) >>= fun cmds -> 
        return (cmd::cmds)

  (** A precondition check (stops early due to short-circuit Boolean evaluation).
    * Accepts the initial state [st] & the command sequnece [cmds] as parameters. *)
  let rec cmds_ok (st : state) (cmds : cmd list) = 
    match cmds with
    | [] -> true
    | c::cs -> Spec.precond c st &&
      let s' = Spec.next_state c st in
        cmds_ok s' cs
  
  (** A generator of command sequences. Accepts the initial state as parameter. *)
  (* TODO: handle shrinking later *)
  let arb_cmds (st : state) = 
    let open Generator.Monad_infix in 
    Int.gen_incl 0 10 >>= fun size -> gen_cmds st size

  (** A consistency test that generates a number of [cmd] sequences and
      checks that all contained [cmd]s satisfy the precondition [precond]. 
      Accepts an optional [count] parameter and a test name as a labeled parameter [name]. *)
  let consistency_test ~trials =
    Quickcheck.test ~trials (arb_cmds Spec.init_state) 
      ~f:(fun cmds -> 
        [%test_result: bool] ~expect:true (cmds_ok Spec.init_state cmds))

  (** Checks agreement between the model and the system under test
   * (stops early, thanks to short-circuit Boolean evaluation). *)
  let rec interp_agree (s : Spec.state) (sut : Spec.sut) (cs : Spec.cmd list) : bool = 
    match cs with
    | [] -> true
    | c::cs ->
      let b = Spec.run_cmd c s sut in
      let s' = Spec.next_state c s in
      b && interp_agree s' sut cs
    

  (** The agreement property: the command sequence [cs] yields the same observations
    * when interpreted from the model's initial state and the [sut]'s initial state.
    * Cleans up after itself by calling [Spec.cleanup] *)
  let agree_prop (cs : Spec.cmd list) : bool =
    assert (cmds_ok Spec.init_state cs);
    let sut = Spec.init_sut () in (* reset system's state *)
    let res = interp_agree Spec.init_state sut cs in
    let ()  = Spec.cleanup sut in
    res
    (* if res then Ok res 
    else Or_error.error "Model & SUT don't agree!" res Bool.sexp_of_t *)
  
  (** An actual agreement test (for convenience). Accepts an optional count parameter
   * and a test name as a labeled parameter [name]. *)
  let agree_test ~trials =
    Quickcheck.test ~trials (arb_cmds Spec.init_state) 
      ~f:(fun cmds -> 
        [%test_result: bool] ~expect:true (agree_prop cmds))
end


module Make2 (Spec : Spec2) : (Harness2 with 
  type state := Spec.state and 
  type cmd := Spec.cmd and 
  type sutA := Spec.sutA and 
  type sutB := Spec.sutB) = struct

  open Spec

  (** A fueled command list generator.
      Accepts a state parameter to enable state-dependent [cmd] generation. *)
  let rec gen_cmds (st : state) (fuel : int) : cmd list Generator.t =
    let open Quickcheck.Generator in 
    if fuel = 0 then return []
    else Spec.gen_cmd st >>= fun cmd ->
	   (gen_cmds (Spec.next_state cmd st) (fuel - 1)) >>= fun cmds -> 
        return (cmd::cmds)

  (** A precondition check (stops early due to short-circuit Boolean evaluation).
      Accepts the initial state [st] & the command sequnece [cmds] as parameters. *)
  let rec cmds_ok (st : state) (cmds : cmd list) = 
    match cmds with
    | [] -> true
    | c::cs -> Spec.precond c st &&
      let s' = Spec.next_state c st in
        cmds_ok s' cs
  
  (** A generator of command sequences. Accepts the initial state as parameter. *)
  (* TODO: handle shrinking later *)
  let arb_cmds (st : state) = 
    let open Generator.Monad_infix in 
    Int.gen_incl 0 10 >>= fun size -> gen_cmds st size

  (** A consistency test that generates a number of [cmd] sequences and
      checks that all contained [cmd]s satisfy the precondition [precond]. 
      Accepts an optional [count] parameter and a test name as a labeled parameter [name]. *)
  let consistency_test ~trials =
    Quickcheck.test ~trials (arb_cmds Spec.init_state) 
      ~f:(fun cmds -> 
        [%test_result: bool] ~expect:true (cmds_ok Spec.init_state cmds))

  (** Checks agreement between the model and the system under test
      (stops early, thanks to short-circuit Boolean evaluation). *)
  let rec interp_agree (st : state) (sutA : sutA) (sutB : sutB) (cmds : cmd list) : bool = 
    match cmds with 
    | [] -> true 
    | c :: cs -> 
      let b = Spec.compare_cmd c st sutA sutB in
      let s' = Spec.next_state c st in
      b && interp_agree s' sutA sutB cs 
  
  (** Agreement property: checks if the command sequence [cs] yields the same observations
    when interpreted from the model's initial state and the [sut]'s initial state.
    Cleans up after itself by calling relevant functions in other modules. *)   
  let agree_prop (cmds : cmd list) : bool = 
    assert (cmds_ok Spec.init_state cmds);
    (* reset system's state *)
    let sutA = Spec.init_sutA () in 
    let sutB = Spec.init_sutB () in 
    let res = interp_agree Spec.init_state sutA sutB cmds in
    Spec.cleanupA sutA;
    Spec.cleanupB sutB;
    res

end
