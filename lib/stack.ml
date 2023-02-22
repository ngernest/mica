open Base

module type Stack = sig
  type 'a t

  include Invariant.S1 with type 'a t := 'a t

  (** [is_empty t] returns true if the stack is empty, else false *)
  val is_empty : 'a t -> bool

  (** [push t a] adds [a] to the top of stack [t]. *)
  val push : 'a -> 'a t -> 'a t

  (** [top t] returns [Some a], where [a] is the top of [t], unless [is_empty t], in which
      case [top] returns [None]. *)
  val top : 'a t -> 'a option

  (** [pop t] removes and returns the top element of [t] as [Some a], or returns [None] if
      [t] is empty. *)
  val pop : 'a t -> 'a t option

  (** [length t] returns the size of the stack *)
  val length : 'a t -> int 
end

