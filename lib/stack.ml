(** Adapted from:
    Core.Stack source code
    https://ocaml.janestreet.com/ocaml-core/109.20.00/doc/core/Stack.html 
    CS 3110 Book Chapter 5.2 & 5.4
    https://cs3110.github.io/textbook/chapters/modules/modules.html 
    https://cs3110.github.io/textbook/chapters/modules/encapsulation.html#abstract-types

    *)



module type Stack = sig
  type 'a t

  (** [empty] is the empty stack. *)
  val empty : 'a t

  (** [create ()] returns an empty stack. *)
  val create : unit -> 'a t

  (** [push a t] adds [a] to the top of stack [t]. *)
  val push : 'a -> 'a t -> 'a t

  (** [pop t] removes and returns the top element of [t] as [Some a], 
      or returns [None] if [t] is empty. *)
  val pop : 'a t -> 'a option

  (** [peek t] returns [Some a], where [a] is the top of [t], unless [is_empty t], in which
      case [top] returns [None]. *)
  val peek : 'a t -> 'a option
  
  (** [clear t] discards all elements from [t] *)
  val clear : 'a t -> unit 

  (** [is_empty t] returns true if the stack is empty, else false *)
  val is_empty : 'a t -> bool
  
  (** [length t] returns the size of the stack *)
  val length : 'a t -> int 

  (** [rep_ok s] checks if the representation invariant holds for [s],
      and throws an exception if the RI doesn't hold *)
  val rep_ok : 'a t -> 'a t 
end

(** Implementation of stacks using linked lists *)
module ListStack : Stack = struct 
    type 'a t = 'a list 

    let empty = []

    let create () = empty

    let push x s = x :: s

    let peek s = 
      match s with 
      | [] -> None
      | x :: _ -> Some x
    
    let pop s = 
      match s with 
      | [] -> None
      | x :: _ -> Some x

    let is_empty s = match s with 
      | [] -> true 
      | _ -> false 

    let length s = List.length s

    let clear _ = ()

    let rep_ok s = s
      (* match s with 
      | [] -> s 
      | x :: xs -> failwith "TODO" *)
end 

(** Implementaiton of stacks using variants *)
(* TODO: make this implement the [Stack] signature defined above *)
module VariantStack = struct
  type 'a t = E | S of 'a * 'a t

  exception Empty 
  let empty = E
  let push x s = S (x, s)
  let peek = function E -> raise Empty | S (x, _) -> x
  let pop = function E -> raise Empty | S (_, s) -> s

  let clear = failwith ""
  let create = failwith ""
  let is_empty s = s = E
  let length = failwith ""
  let rep_ok = failwith ""
end


(** Pathological implementation of stacks using record options, from CS 3110 *)
module CustomStack : Stack = struct 
  type 'a entry = {top : 'a; rest : 'a t; size : int}
  and 'a t = S of 'a entry option

  let empty = S None

  let is_empty cs = 
    match cs with 
    | S None -> true 
    | _ -> false


  let size cs = 
    match cs with 
    | S None -> 0 
    | S (Some {size; _}) -> size
  
  let push x s = S (Some {top = x; rest = s; size = size s + 1})
  
  let peek cs = 
    match cs with 
    | S None -> None 
    | S (Some {top; _}) -> Some top
  
  let pop cs = 
    match cs with 
    | S None -> None
    | S (Some {rest; _}) -> failwith "TODO"

  let create = failwith ""

  let length = failwith ""

  let rep_ok = failwith ""

  let clear = failwith ""

end