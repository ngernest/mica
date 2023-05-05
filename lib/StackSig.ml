(** Adapted from:
    
    Core.Stack source code
    https://ocaml.janestreet.com/ocaml-core/109.20.00/doc/core/Stack.html 
    
    Cornell CS3110 Book, Chapter 5.2 & 5.4
    https://cs3110.github.io/textbook/chapters/modules/modules.html 
    https://cs3110.github.io/textbook/chapters/modules/encapsulation.html#abstract-types

*)



module type StackIntf = sig
  type 'a t
    [@@deriving sexp]

  (** [empty] is the empty stack. *)
  val empty : 'a t

  (** [create ()] returns an empty stack. *)
  val create : unit -> 'a t

  (** [push a t] adds [a] to the top of stack [t]. *)
  val push : 'a -> 'a t -> 'a t

  (** [pop t] removes the top element of [t], 
      and returns the remaininder of the stack, 
      or [None] if [t] is empty *)
  val pop : 'a t -> 'a t option

  (** [peek t] returns [Some a], where [a] is the top of [t], unless [is_empty t], in which
      case [top] returns [None]. *)
  val peek : 'a t -> 'a option
  
  (** [clear t] discards all elements from [t] *)
  val clear : 'a t -> unit 

  (** [is_empty t] returns true if the stack is empty, else false *)
  val is_empty : 'a t -> bool
  
  (** [length t] returns the size of the stack *)
  val length : 'a t -> int 
  
  (** [invariant x s] is true if the invariant [pop (push x s) = s] holds 
      for any randomly generated [x] *)
  (* val invariant : 'a -> 'a t -> bool  *)

  (** [rep_ok s] checks if the representation invariant holds for [s],
      and throws an exception if the RI doesn't hold *)
  (* val rep_ok : 'a t -> 'a t  *)
end

(** Implementation of stacks using linked lists *)
module ListStack : StackIntf = struct 
    type 'a t = 'a Base.List.t
      [@@deriving sexp]

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
      | _ :: s' -> Some s'
    (* Manufacture a bug: by doing [| _ -> Some s] in the pattern match *)  

    let is_empty s = 
      match s with 
      | [] -> true 
      | _ -> false 

    let length s = List.length s

    let clear _ = ()

    (* Tentative invariant *)
    (* let invariant x s = 
      Option.value (pop (push x s)) ~default:empty = s *)
end 

(** Implementation of stacks using variants *)
module VariantStack : StackIntf = struct
  type 'a t = 
    | Nil 
    | Cons of 'a * 'a t
  [@@deriving sexp, compare]

  let empty = Nil
  let push x s = Cons (x, s)

  let peek = function Nil -> None | Cons (x, _) -> Some x
  let pop = function Nil -> None | Cons (_, xs) -> Some xs

  let clear _ = ()

  let create () = empty

  let is_empty s = 
    let open Core.Poly in 
    s = Nil
    
  let rec length s =
    match s with 
    | Nil -> 0
    | Cons (_, xs) -> 1 + length xs

  (* Tentative invariant *)
  (* let invariant x s = 
    Option.value (pop (push x s)) ~default:empty = s *)

end


(** Pathological implementation of stacks using record options, from CS 3110 *)
(* module CustomStack : StackIntf = struct 
  type 'a entry = {top : 'a; rest : 'a t; size : int}
  and 'a t = S of 'a entry option
    [@@deriving sexp]

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
    | S (Some _) -> failwith "TODO"

  let create = failwith ""

  let length = failwith ""

  let rep_ok = failwith ""

  let clear = failwith ""

end *)