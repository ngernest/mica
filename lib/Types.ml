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

  (** [interp expr] interprets the expression [expr] wrt the module 
      implementation [M], and returns the result as a [value] *)     
  let rec interp (expr : expr) : value = 
    match expr with 
    | EInt n -> ValInt n
    | Empty -> ValT M.empty
    (* TODO: make the nested pattern-matches more succint *)
    | Add (x, e) -> 
      (match interp e with 
      | ValT v -> ValT (M.add x v)
      | _ -> failwith "ill-typed")
    | Remove (x, e) -> 
      (match interp e with 
      | ValT v -> ValT (M.rem x v)
      | _ -> failwith "ill-typed")
    | Union (e1, e2) -> 
      (match interp e1, interp e2 with 
      | ValT v1, ValT v2 -> ValT (M.union v1 v2)
      | _ -> failwith "ill-typed")
    | Intersect (e1, e2) ->
      (match interp e1, interp e2 with 
      | ValT v1, ValT v2 -> ValT (M.inter v1 v2)
      | _ -> failwith "ill-typed")
    | Mem (x, e) -> 
      (match interp e with 
      | ValT v -> ValBool (M.mem x v)
      | _ -> failwith "ill-typed")
    | Size e -> 
      (match interp e with 
      | ValT v -> ValInt (M.size v)
      | _ -> failwith "ill-typed")
    | Is_empty e ->
      (match interp e with 
      | ValT v -> ValBool (M.is_empty v)
      | _ -> failwith "ill-typed")

end

(** Generator for expressions: 
  * [gen_expr ty] produces an generator of [expr]s that have return type [ty] *)  
let rec gen_expr (ty : ty) : expr Generator.t = 
  let module G = Generator in 
  let open Generator.Let_syntax in 
  match ty with 
  | Int -> 
      let intExpr = 
        let%map n = G.int_uniform in 
          EInt n in 
      let size = 
        let%map e = gen_expr T in 
          (Size e) in 
      G.union [intExpr; size]
  | Bool -> 
    let mem = 
      let%bind x = G.int_uniform in
      let%map e = gen_expr T in 
        Mem (x, e) in
    let isEmpty = 
      let%map e = gen_expr T in 
        Is_empty e in
      G.union [mem; isEmpty]
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
    G.union [add; remove; union; intersect]


module I1 = ExprToImpl(ListSetNoDups)
module I2 = ExprToImpl(ListSetDups)
    

(** TODO: not sure if [test] in [Core.Quickcheck] is a perfect substitute for 
    QC.forall in Haskell *)

(** TODO: for some reason, Dune doesn't like the [let%test_unit] annotation *)
let%test_unit "bool_expr" = Core.Quickcheck.test (gen_expr Bool) ~f:(fun e -> 
  match I1.interp e, I2.interp e with 
  | ValBool b1, ValBool b2 -> 
      [%test_eq: bool] b1 b2
  | ValInt n1, ValInt n2 -> 
      [%test_eq: int] n1 n2
  | _, _ -> (failwith "ill-typed"))

    
    
  

