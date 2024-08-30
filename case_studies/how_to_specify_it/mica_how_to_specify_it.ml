open Core
open Quickcheck
open All_bsts
open Bst

(******************************************************************************)
(** The following is very similar to the code that Mica would generate
   automatically. The only manual modifications are: 
   - Variable renaming
   - The use of [Base_quickcheck]'s [int_inclusive] generator
   (to generate integers in the range [0] to [size], where [size] is the 
   size parameter of Mica's internal QuickCheck generator)
   - certain [let open] statements to prevent namespace clashes *)

module Mica = struct
  type expr =
    | Nil
    | Find of int * expr
    | Insert of int * int * expr
    | Delete of int * expr
    | Union of expr * expr
    | ToList of expr
    | Keys of expr
    | Size of expr
  [@@deriving show { with_path = false }]

  type ty = T | Int | IntList | IntIntList | IntOption

  let rec gen_expr (ty : ty) : expr Generator.t =
    let open Base_quickcheck.Generator in
    let open Let_syntax in
    let%bind k = size in
    match (ty, k) with
    | Int, _ ->
      let%map t = with_size ~size:(k / 2) (gen_expr T) in
      Size t
    | IntList, _ ->
      let%map t = with_size ~size:(k / 2) (gen_expr T) in
      Keys t
    | IntIntList, _ ->
      let%map t = with_size ~size:(k / 2) (gen_expr T) in
      ToList t
    | IntOption, _ ->
      let%map k = int_inclusive 0 k
      and t = with_size ~size:(k / 2) (gen_expr T) in
      Find (k, t)
    | T, 0 -> return Nil
    | T, _ ->
      let gen_insert =
        let%map k = int_inclusive 0 k
        and v = int_inclusive 0 k
        and t = with_size ~size:(k / 2) (gen_expr T) in
        Insert (k, v, t) in
      let gen_delete =
        let%map k = int_inclusive 0 k
        and t = with_size ~size:(k / 2) (gen_expr T) in
        Delete (k, t) in
      let gen_union =
        let%map t1 = with_size ~size:(k / 2) (gen_expr T)
        and t2 = with_size ~size:(k / 2) (gen_expr T) in
        Union (t1, t2) in
      union [ gen_insert; gen_delete; gen_union ]

  module Interpret (M : BST) = struct
    type value =
      | ValInt of int
      | ValIntOption of int option
      | ValIntList of int list
      | ValIntIntList of (int * int) list
      | ValT of (int, int) Bst.t
    [@@deriving show { with_path = false }]

    let rec interp (expr : expr) : value =
      match expr with
      | Nil -> ValT M.nil
      | Find (k, e) -> (
        match interp e with
        | ValT t -> ValIntOption (M.find k t)
        | _ -> failwith "impossible")
      | Insert (k, v, e) -> (
        match interp e with
        | ValT t -> ValT (M.insert k v t)
        | _ -> failwith "impossible")
      | Delete (k, e) -> (
        match interp e with
        | ValT t -> ValT (M.delete k t)
        | _ -> failwith "impossible")
      | Union (e1, e2) -> (
        match (interp e1, interp e2) with
        | ValT t1, ValT t2 -> ValT (M.union t1 t2)
        | _ -> failwith "impossible")
      | ToList e -> (
        match interp e with
        | ValT t -> ValIntIntList (M.toList t)
        | _ -> failwith "impossible")
      | Keys e -> (
        match interp e with
        | ValT t -> ValIntList (M.keys t)
        | _ -> failwith "impossible")
      | Size e -> (
        match interp e with
        | ValT t -> ValInt (M.size t)
        | _ -> failwith "impossible")
  end

  module TestHarness (M1 : BST) (M2 : BST) = struct
    module I1 = Interpret (M1)
    module I2 = Interpret (M2)
    open Core

    let test_int () : unit =
      Quickcheck.test (gen_expr Int) ~f:(fun e ->
          match (I1.interp e, I2.interp e) with
          | ValInt n1, ValInt n2 -> [%test_eq: int] n1 n2
          | v1, v2 -> failwith "failed int")

    let test_int_option () : unit =
      Quickcheck.test (gen_expr IntOption) ~f:(fun e ->
          match (I1.interp e, I2.interp e) with
          | ValIntOption n1, ValIntOption n2 -> [%test_eq: int option] n1 n2
          | v1, v2 -> failwith "failed int option")

    let test_int_list () : unit =
      Quickcheck.test (gen_expr IntList) ~f:(fun e ->
          match (I1.interp e, I2.interp e) with
          | ValIntList xs, ValIntList ys -> [%test_eq: int list] xs ys
          | _ -> failwith "failed int list")

    let test_int_int_list () : unit =
      Quickcheck.test (gen_expr IntIntList) ~f:(fun e ->
          match (I1.interp e, I2.interp e) with
          | ValIntIntList xs, ValIntIntList ys ->
            [%test_eq: (int * int) list] xs ys
          | _ -> failwith "failed (int * int) list")

    let run_tests () : unit =
      test_int ();
      test_int_option ();
      test_int_list ();
      test_int_int_list ()
  end
end

(******************************************************************************)
(* Some (manually-written) test harness boilerplate below *)

(** Wrapper function that takes some (possibly buggy) BST module [M] and 
    tests it against the module [BST0], which is known to be correct *)
let bug_test (module M : BST) : unit =
  let module T = Mica.TestHarness (BST0) (M) in
  T.run_tests ()

(** The main executable code: performs differential testing of all 8 buggy 
    BST implementations with [BST0] (an implementation known to be correct) *)
let () =
  printf "Catching bug 1:\n";
  bug_test (module BST1);
  printf "Catching bug 2:\n";
  bug_test (module BST2);
  printf "Catching bug 3:\n";
  bug_test (module BST3);
  printf "Catching bug 4:\n";
  bug_test (module BST4);
  printf "Catching bug 5:\n";
  bug_test (module BST5);
  printf "Catching bug 6:\n";
  bug_test (module BST6);
  printf "Catching bug 7:\n";
  bug_test (module BST7);
  printf "Catching bug 8:\n";
  bug_test (module BST8)
