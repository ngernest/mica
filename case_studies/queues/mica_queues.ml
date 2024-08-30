open Queue_impls
open Base_quickcheck

(******************************************************************************)
(** The following is very similar to the code that Mica would generate
   automatically. The only manual modifications are: 
   - Variable renaming 
   - The introduction of the extra [Seq] constructor for the [expr] datatype, which
   represents the {i sequencing} of two symbolic expressions (see the paper for
   more details). *)

module Mica = struct
  type expr =
    | Create of unit
    | Length of expr
    | Is_empty of expr
    | Mem of expr * int
    | To_list of expr
    | Enqueue of expr * int
    | Dequeue of expr
    | Peek of expr
    | Min_elt of expr
    | Seq of expr * expr
  [@@deriving show { with_path = false }]

  type ty = T | Unit | Bool | Int | IntList | IntOption

  let rec gen_expr (ty : ty) : expr Generator.t =
    let module G = Generator in
    let open G.Let_syntax in
    let%bind k = G.size in
    match (ty, k) with
    | T, _ -> G.return (Create ())
    | Unit, 0 ->
      let%map x = G.int and e = G.with_size ~size:(k / 2) (gen_expr T) in
      Enqueue (e, x)
    | Unit, _ ->
      let%map pre = G.with_size ~size:(k / 2) (gen_expr Unit)
      and x = G.int
      and e = G.with_size ~size:(k / 2) (gen_expr T) in
      Seq (pre, Enqueue (e, x))
    | Int, 0 ->
      let%map e = G.with_size ~size:(k / 2) (gen_expr T) in
      Length e
    | Int, _ ->
      let%map pre = G.with_size ~size:(k / 2) (gen_expr Unit)
      and e = G.with_size ~size:(k / 2) (gen_expr T) in
      Seq (pre, Length e)
    | IntList, 0 ->
      let%map e = G.with_size ~size:(k / 2) (gen_expr T) in
      To_list e
    | IntList, _ ->
      let%map pre = G.with_size ~size:(k / 2) (gen_expr Unit)
      and e = G.with_size ~size:(k / 2) (gen_expr T) in
      Seq (pre, To_list e)
    | Bool, 0 ->
      let%bind e = G.with_size ~size:(k / 2) (gen_expr T) and x = G.int in
      G.union [ G.return @@ Is_empty e; G.return @@ Mem (e, x) ]
    | Bool, _ ->
      let%bind pre = G.with_size ~size:(k / 2) (gen_expr Unit)
      and e = G.with_size ~size:(k / 2) (gen_expr T)
      and x = G.int in
      G.union
        [ G.return @@ Seq (pre, Is_empty e); G.return @@ Seq (pre, Mem (e, x)) ]
    | IntOption, 0 ->
      let%bind e = G.with_size ~size:(k / 2) (gen_expr T) in
      G.union
        [ G.return @@ Dequeue e; G.return @@ Peek e; G.return @@ Min_elt e ]
    | IntOption, _ ->
      let%bind pre = G.with_size ~size:(k / 2) (gen_expr Unit)
      and e = G.with_size ~size:(k / 2) (gen_expr T) in
      G.union
        [ G.return @@ Seq (pre, Dequeue e);
          G.return @@ Seq (pre, Peek e);
          G.return @@ Seq (pre, Min_elt e)
        ]

  module Interpret (M : S) = struct
    type value =
      | ValBool of bool
      | ValInt of int
      | ValUnit of unit
      | ValIntOption of int option
      | ValIntList of int list
      | ValIntT of int M.t

    let rec interp (expr : expr) : value =
      match expr with
      | Create () -> ValIntT (M.create ())
      | Length e -> (
        match interp e with
        | ValIntT e' -> ValInt (M.length e')
        | _ -> failwith "impossible")
      | Is_empty e -> (
        match interp e with
        | ValIntT e' -> ValBool (M.is_empty e')
        | _ -> failwith "impossible")
      | Mem (e, x) -> (
        match interp e with
        | ValIntT e' -> ValBool (M.mem e' x)
        | _ -> failwith "impossible")
      | To_list e -> (
        match interp e with
        | ValIntT e' -> ValIntList (M.to_list e')
        | _ -> failwith "impossible")
      | Enqueue (e, x) -> (
        match interp e with
        | ValIntT e' -> ValUnit (M.enqueue e' x)
        | _ -> failwith "impossible")
      | Dequeue e -> (
        match interp e with
        | ValIntT e' -> ValIntOption (M.dequeue e')
        | _ -> failwith "impossible")
      | Peek e -> (
        match interp e with
        | ValIntT e' -> ValIntOption (M.peek e')
        | _ -> failwith "impossible")
      | Min_elt e -> (
        match interp e with
        | ValIntT e' -> ValIntOption (M.min_elt e')
        | _ -> failwith "impossible")
      | Seq (e1, e2) -> (
        match interp e1 with
        | ValUnit () -> interp e2
        | _ -> failwith "expected [interp e1] to return [ValUnit]")
  end

  module TestHarness (M1 : S) (M2 : S) = struct
    module I1 = Interpret (M1)
    module I2 = Interpret (M2)
    open Core

    let test_bool () : unit =
      Quickcheck.test (gen_expr Bool) ~f:(fun e ->
          match (I1.interp e, I2.interp e) with
          | ValBool b1, ValBool b2 -> [%test_eq: bool] b1 b2
          | _ -> failwith "failed bool")

    let test_int () : unit =
      Quickcheck.test (gen_expr Int) ~f:(fun e ->
          match (I1.interp e, I2.interp e) with
          | ValInt i1, ValInt i2 -> [%test_eq: int] i1 i2
          | _ -> failwith "failed int")

    let test_int_option () : unit =
      Quickcheck.test (gen_expr IntOption) ~f:(fun e ->
          match (I1.interp e, I2.interp e) with
          | ValIntOption i1, ValIntOption i2 -> [%test_eq: int option] i1 i2
          | _ -> failwith "failed int")

    let test_int_list () : unit =
      Quickcheck.test (gen_expr IntList) ~f:(fun e ->
          match (I1.interp e, I2.interp e) with
          | ValIntList l1, ValIntList l2 -> [%test_eq: int list] l1 l2
          | _ -> failwith "failed int")

    let run_tests () : unit =
      test_bool ();
      test_int ();
      test_int_option ();
      test_int_list ();
      printf "Mica: OK, passed %d observational equivalence tests.\n" 40000
  end
end

(******************************************************************************)
(* Using Mica to check observational equivalence of two implementations of
   persistent queues below: *)

module T = Mica.TestHarness (Queue) (Linked_queue)

let () = T.run_tests ()
