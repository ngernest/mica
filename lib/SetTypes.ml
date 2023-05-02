open Base
open Base_quickcheck 
open Sets

(** Revised definition of expressions *)
type expr = 
  (* Base cases *)
  | Empty
  | EInt of int
  (* Commands that change the underlying state *)
  | Add of int * expr
  | Remove of int * expr
  | Union of expr * expr 
  | Intersect of expr * expr 
  (* Observable commands *)
  | Mem of int * expr 
  | Size of expr
  | Is_empty of expr  
  [@@deriving sexp_of, quickcheck]

(** ADT representing types, where [T] is the abstract type of the module *)
type ty = T | Int | Bool
  [@@deriving sexp_of, quickcheck]



(** Functor relating [expr] to the actual module [M] implementing [SetIntf] *)
module ExprToImpl (M : SetIntf) = struct 

  include M
  
  (** Type representing values, i.e. observations made *)
  type value = ValT of int M.t | ValInt of int | ValBool of bool
    [@@deriving sexp_of]

  (** [interp expr] interprets the expression [expr] wrt the module 
      implementation [M], and returns the result as a [value] *)     
  let rec interp (expr : expr) : value Or_error.t = 
    let open Or_error.Let_syntax in 
    match expr with 
    | EInt n -> return (ValInt n)
    | Empty -> return (ValT M.empty)
    | Add (x, e) -> interp_helper e (M.add x)
    | Remove (x, e) -> interp_helper e (M.rem x)
    | Union (e1, e2) -> 
      (match Or_error.both (interp e1) (interp e2) with 
      | Ok (ValT v1, ValT v2) -> return @@ ValT (M.union v1 v2)
      | Ok malformed -> Or_error.error "ill-typed" malformed [%sexp_of: value * value]
      | Error _ as err -> Or_error.tag err ~tag:"Failed to evaluate one or more arguments to union")
    | Intersect (e1, e2) ->
      (match Or_error.both (interp e1) (interp e2) with 
      | Ok (ValT v1, ValT v2) -> return @@ ValT (M.intersection v1 v2)
      | Ok malformed -> Or_error.error "ill-typed" malformed [%sexp_of: value * value]
      | Error _ as err -> Or_error.tag err ~tag:"Failed to evaluate one or more arguments to intersection")
    | Mem (x, e) -> 
      (match interp e with 
      | Ok ValT v -> 
        let%map v' = M.invariant v in
        ValBool (M.mem x v')
      | Ok malformed -> Or_error.error "ill-typed" malformed sexp_of_value 
      | Error _ as err -> Or_error.tag err ~tag: "interp_helper failed")
    | Size e -> 
      (match interp e with 
      | Ok ValT v -> 
        let%map v' = M.invariant v in 
        ValInt (M.size v')
      | Ok malformed -> Or_error.error "ill-typed" malformed sexp_of_value 
      | Error _ as err -> Or_error.tag err ~tag: "interp_helper failed")
    | Is_empty e ->
      (match interp e with 
      | Ok ValT v -> 
        let%map v' = M.invariant v in 
        ValBool (M.is_empty v')
      | Ok malformed -> Or_error.error "ill-typed" malformed sexp_of_value 
      | Error _ as err -> Or_error.tag err ~tag: "interp_helper failed")

    (** [interp_helper e f] interprets the expression [e], and applies 
        [f] to the resultant value [v] if [interp e] returns [Ok ValT v] *)
    and interp_helper (e : expr) (f: 'a t -> 'a t) : value Or_error.t = 
      let open Or_error.Let_syntax in 
      match interp e with 
      | Ok ValT v ->
        let%map v' = M.invariant v in 
        ValT (f v')
      | Ok malformed -> Or_error.error "ill-typed" malformed sexp_of_value 
      | Error _ as err -> Or_error.tag err ~tag: "interp_helper failed"  
end

(** Generator for expressions: 
  * [gen_expr ty] produces an generator of [expr]s that have return type [ty] *)  

let rec gen_expr (ty : ty) : expr Generator.t = 
  let module G = Generator in 
  let open G.Let_syntax in 
  match ty with 
  | Int -> 
      (* Note how we've remove the case for [EInt]*)
      let%map e = gen_expr T in Size e
  
  | Bool -> 
    let mem = 
      let%bind x = G.int_uniform in
      let%map e = gen_expr T in 
        Mem (x, e) in
    let isEmpty = 
      let%map e = gen_expr T in 
        Is_empty e in
      G.weighted_union [(3.0, mem); (1.0, isEmpty)]
  
  | T -> 
    let add = 
      let%bind x = G.int_uniform in 
      let%map e = gen_expr T in 
        Add (x, e) in 
    let remove = 
      let%bind x = G.int_uniform in 
      let%map e = gen_expr T in 
        Remove (x, e) in 
    let union = 
      let%bind e1 = gen_expr T in 
      let%bind e2 = gen_expr T in 
        G.return @@ Union (e1, e2) in 
    let intersect = 
      let%bind e1 = gen_expr T in 
      let%bind e2 = gen_expr T in 
        G.return @@ Intersect (e1, e2) in 
    G.weighted_union [
      (3.0, add); 
      (2.0, remove); 
      (3.0, union); 
      (2.0, intersect)
    ]

(** Calling [gen_expr T] should always generate a tree 
  * [gen_expr Int] should be "give me a tree abstract type [t], and give me an [int]"    
  * [gen_expr] is always gonna go through the abstract type [T]
  * the test case corresponding to [size] should take the size of a tree after a bunch of unions/adds
  * When implementing the examples, do the ones that throw exceptions first (instead of ones that use Maybe)
  * think about what it woudl take to go from the sig -> generating the PBT code
  * 
*)    

module I1 = ExprToImpl(ListSetNoDups)
module I2 = ExprToImpl(ListSetDups)
    

(** TODO: not sure if [test] in [Core.Quickcheck] is a perfect substitute for 
    QC.forall in Haskell *)

(** TODO: for some reason, Dune doesn't like the [let%test_unit] annotation
  * TODO: fix this in Dune *)
(* let%test_unit "bool_expr" = Core.Quickcheck.test (gen_expr Bool) ~f:(fun e -> 
  match Or_error.both (I1.interp e) (I2.interp e) with 
  | Ok (ValBool b1, ValBool b2) -> 
      [%test_eq: bool] b1 b2
  | Ok _ -> failwith "Generated ill-typed value"
  | Error _ -> failwith "Invariant failed") *)

(* let%expect_test "bool_expr" = Core.Quickcheck.test (gen_expr Bool) ~f:(fun e -> 
  match I1.interp e, I2.interp e with 
  | ValBool b1, ValBool b2 -> 
      [%test_eq: bool] b1 b2
  | _, _ -> (failwith "ill-typed"));
  [%expect {| () |}]  *)
  
    
  

