(** Adapted from Core.Stack
    https://ocaml.janestreet.com/ocaml-core/109.20.00/doc/core/Stack.html *)

open! Core

module type Stack = sig
  type 'a t

  (** Currently, we include the Base Invariant module, although this 
      hasn't been integrated with the rest of the module yet -- 
      this is also the case in the actual Core source code *)
  include Invariant.S1 with type 'a t := 'a t

  (** [push t a] adds [a] to the top of stack [t]. *)
  val push : 'a -> 'a t -> 'a t

  (** [pop t] removes and returns the top element of [t] as [Some a], 
      or returns [None] if [t] is empty. *)
  val pop : 'a t -> 'a t option

  (** [top t] returns [Some a], where [a] is the top of [t], unless [is_empty t], in which
      case [top] returns [None]. *)
  val top : 'a t -> 'a option
  
  (** [clear t] discards all elements from [t] *)
  val clear : 'a t -> unit 

  (** [is_empty t] returns true if the stack is empty, else false *)
  val is_empty : 'a t -> bool
  
  (** [length t] returns the size of the stack *)
  val length : 'a t -> int 
end

