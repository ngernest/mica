(** Code associated with the paper 
    "ArtiCheck: well-typed generic fuzzing for module interfaces",
    Braibant et al, 2014.
*)

open! List


(** SECTION 2 *)

(** Signature & implementation for sorted integer lists *)
module SIList: sig
  type t
  val empty: t
  val add: int -> t -> t

  (* Invariant that the list should always remain sorted *)
  val sorted: t -> bool
end = struct
  type t = int list
  let empty = []
  let rec add x = function
    | [] -> [x]
    | t::q -> if t<x then t::add x q else x::t::q
  let rec sorted = function
    | [] | [_] -> true
    | t1::(t2::_ as q) -> t1 <= t2 && sorted q
end



(** The type descriptor ['a ty] keeps track of all the instances of 
    ['a] we have created so far in [enum] .
    The function [fresh] generates a new instance of ['a] *)
type 'a ty = { 
  mutable enum: 'a list;
  fresh: ('a list -> 'a) option; 
}

(** The GADT [('a, 'b) fn] describes ways to generate instances of type ['b] 
    using a function of arrow type ['a] *)
type (_,_) fn =
  | Ret: 'a ty -> ('a,'a) fn
  | Fun: 'a ty * ('b, 'c) fn -> ('a -> 'b, 'c) fn 
  (* Helpers for creating [fn]'s. *)
  let (@->) ty fd = Fun (ty,fd)
  let returning ty = Ret ty


(** [eval] takes a function descriptor [fd], recurses over it, 
    thereby refining the type of its argument [f]. 
    
    The use of GADTs allows us to statically prove that the [eval] function only ever produces instances of type [b]. *)
let rec eval : type a b. (a,b) fn -> a -> b list =
  fun fd f ->
    match fd with
    | Ret _ -> [f]
    | Fun (ty,fd) -> List.flatten (
        List.map (fun e -> eval fd (f e)) ty.enum)

(** [codom] finds the type descriptor associated with the return value (codomain)
    of an [fn] *)        
let rec codom : type a b. (a,b) fn -> b ty =
function
    | Ret ty -> ty
    | Fun (_,fd) -> codom fd  

(** [use] takes a function descriptor [fd] along with a matching function [f].
    The [prod] variable contains all instances of ['b] that 
    we just managed to create, [ty] is the type descriptor of ['b]. 

    We store the new instances of ['b] in the corresponding type descriptor. *)
let use (fd: ('a, 'b) fn) (f: 'a): unit =
  let prod, ty = eval fd f, codom fd in
  List.iter (fun x ->
    if not (List.mem x ty.enum)
    then ty.enum <- x::ty.enum
  ) prod    



(** Signature descriptors 
    An entry in a signature descriptor is a function of arrow type ['a] along with its corresponding function descriptor. *)

type sig_elem = Elem : ('a,'b) fn * 'a -> sig_elem 
type sig_descr = (string * sig_elem) list

(********************************************************************)
(** SECTION 3 *)


(** The GADT [('a, 'b) neg] (negative) represents a {i computation}
    of type ['a] that produces a result of type ['b]. 

    The GADT ['a pos] (positive) represents a {i value}, 
    i.e. the result of a computation. 
    The [pos] type represents 1st-order data types (products, sums & atomic types), i.e. whatever is on the rightmost side of an arrow. 

    The [Ret] constructor provides an injection from positive
    to negative types. (A value of type ['a] is a constant computation.)    

    NB: The authors {i don't} provide an injection 
    from negative to positive types, which is necessary to model higher-order functions. 

    The [Bij] constructor provides a bijection between 
    a built-in OCaml type (eg. ['a option]) & its ArtiCheck
    representation ([() + 'a]). 
    *)


type (_, _) neg = 
  | Fun : 'a pos * ('b, 'c) neg -> ('a -> 'b, 'c) neg 
  | Ret : 'a pos -> ('a, 'a) neg 
and _ pos = 
  | Ty : 'a ty -> 'a pos 
  | Sum : 'a pos * 'b pos -> ('a, 'b) sum pos 
  | Prod : 'a pos * 'b pos -> ('a * 'b) pos 
  | Bij : 'a pos * ('a, 'b) bijection -> 'b pos 
and ('a, 'b) sum = L of 'a | R of 'b
and ('a, 'b) bijection = ('a -> 'b) * ('b -> 'a)

(** [destruct] breaks down positive types by pattern-matching, 
    populating the descriptions of the various types it encounters
    as it goes. *)
let rec destruct: type a. a pos -> a -> unit =
  function
  (** [remember] records all new instances in [ty] *)
  | Ty ty -> (fun v -> remember v ty)
  | Prod (ta, tb) -> (fun (a, b) ->
      destruct ta a; destruct tb b)    
  | _ -> failwith "omitted"

(** [apply] takes a computation & a matching description, 
    and generates a list of [b]s, relying on [product] to 
    exhaustively exhibit all possible arguments one can 
    pass to the function *)
let rec apply: type a b. (a, b) neg -> a -> b list =
  fun ty v -> match ty with
  | Fun (p, n) ->
      produce p |> concat_map (fun a -> apply n (v a))
  | _ -> failwith "omitted"

(** [produce] exhibits the set of instances we can build
    for any given type *)  
and produce: type a. a pos -> a list =
  fun ty -> match ty with
  | Ty ty -> ty.enum
  | Prod (pa, pb) ->
      let cartesian_product = failwith "omitted" in 
      cartesian_product (produce pa) (produce pb)
  | _ -> failwith "omitted"


(** TODO: read section 3.2 onwards *)