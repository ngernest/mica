open! Base
open! Base_quickcheck
open! StackSig

(** ADT for expressions *)
type expr = 
  (* Base cases *)
  | Empty
  | EChar of char
  (* Commands that change the underlying state *)
  | Push of char * expr
  | Pop of expr 
  | Peek of expr 
  | Clear of expr 
  (* Observable commands *)
  | Is_empty of expr 
  | Length of expr 
  [@@deriving sexp_of, quickcheck]

(** ADT representing types, where [T] is the abstract type of the module *)
type ty = T | Int | Bool 
  [@@deriving sexp_of, quickcheck]  

(** Functor relating [expr] to the actual module [M] implementing [SetIntf] *)
module ExprToImpl (M : StackIntf) = struct 
  
  include M 

  (** Type representing values, i.e. observations made *)
  type value = 
    | ValT of char M.t 
    | ValChar of char 
    | ValBool of bool 
    | ValInt of int

  (** [interp expr] interprets the expression [expr] wrt the module 
      implementation [M], and returns the result as a [value] *)
  (** TODO: experiment with version that throws exceptions? *)      
  let rec interp (expr : expr) : value = 
    match expr with 
    | EChar c -> ValChar c
    | Empty -> ValT M.empty 
    | Push (x, e) -> 
      (match interp e with 
      | ValT v -> ValT (M.push x v)
      | _ -> failwith "ill-typed")
    | Pop e ->
      (match interp e with 
      | ValT v -> ValChar (Option.value (M.pop v) ~default:' ')
      | _ -> failwith "ill-typed")
    | Peek e -> 
      (match interp e with 
      | ValT v -> ValChar (Option.value (M.peek v) ~default:' ')
      | _ -> failwith "ill-typed")
    | Clear e -> 
      (match interp e with 
      | ValT _ -> ValT M.empty
      | _ -> failwith "ill-typed")
    | Is_empty e -> 
      (match interp e with 
      | ValT v -> ValBool (M.is_empty v)
      | _ -> failwith "ill-typed")
    | Length e -> 
      (match interp e with 
      | ValT v -> ValInt (M.length v)
      | _ -> failwith "ill-typed")

(* TODO: work on [gen_expr] *)      
  
end

