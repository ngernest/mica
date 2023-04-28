open! Base
open! Base_quickcheck
open StackSig

(** ADT for expressions *)
type expr = 
  (* Base cases *)
  | Empty
  | EChar of char
  (* Commands that change the underlying state *)
  | Push of char * expr
  | Pop of expr 
  | Peek of expr 
  | Create of unit
  (* Observable commands *)
  | Clear of expr 
  | Is_empty of expr 
  | Length of expr 
  [@@deriving sexp_of, quickcheck]

(** ADT representing the return type of an expression, 
    where [T] is the abstract type of the module *)
type ty = T | Char | Int | Bool | Unit
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
    | ValUnit of unit

  (** [interp expr] interprets the expression [expr] wrt the module 
      implementation [M], and returns the result as a [value] *)
  (** TODO: experiment with version that throws exceptions? *)      
  let rec interp (expr : expr) : value = 
    match expr with 
    | EChar c -> ValChar c
    | Empty -> ValT M.empty 
    | Create () -> ValT M.empty
    | Push (x, e) -> 
      (match interp e with 
      | ValT v -> ValT (M.push x v)
      | _ -> failwith "ill-typed")
    | Pop e ->
      (match interp e with 
      | ValT v -> ValT (Option.value (M.pop v) ~default:M.empty)
      | _ -> failwith "ill-typed")
    | Peek e -> 
      (match interp e with 
      | ValT v -> ValChar (Option.value (M.peek v) ~default:' ')
      | _ -> failwith "ill-typed")
    | Clear e -> 
      (match interp e with 
      | ValT _ -> ValUnit ()
      | _ -> failwith "ill-typed")
    | Is_empty e -> 
      (match interp e with 
      | ValT v -> ValBool (M.is_empty v)
      | _ -> failwith "ill-typed")
    | Length e -> 
      (match interp e with 
      | ValT v -> ValInt (M.length v)
      | _ -> failwith "ill-typed")

end

(** Generator for expressions: 
* [gen_expr ty] produces an generator of [expr]s that have return type [ty] *)  
let rec gen_expr (ty : ty) : expr Generator.t = 
  let module G = Generator in 
  let open G.Let_syntax in 
  match ty with 
  | Int -> let%map e = gen_expr T in 
    Length e
  
  | Bool -> let%map e = gen_expr T in 
    Is_empty e 
  
  | Char -> 
    let pop = 
      let%map e = gen_expr T in Pop e in 
    let peek = 
      let%map e = gen_expr T in Peek e in 
    G.union [pop; peek]

  | Unit -> 
    let%map e = gen_expr T in Clear e

  | T -> 
    let push = 
      let%bind x = G.char_alpha in 
      let%map e = gen_expr T in 
      Push (x, e) in 
    G.union [push; G.return @@ Create ()]

module I1 = ExprToImpl(ListStack)
module I2 = ExprToImpl(VariantStack)

let%test_unit "bool_expr" = Core.Quickcheck.test (gen_expr Bool) 
  ~f:(fun e -> 
  match I1.interp e, I2.interp e with 
  | ValBool b1, ValBool b2 ->
      [%test_eq: bool] b1 b2
  | _, _ -> failwith "ill-typed")