open Base
open Base_quickcheck
open Charsets
open Generator
module M1 = Stdlib_Charset
module M2 = Yallop_Charset

(******************************************************************************)
(** The following is very similar to the code that Mica would generate
   automatically. The only manual modifications are that we annotate the 
   [expr] type with [@@deriving sexp_of] and pass [sexp_of_expr] as an optional
   argument to [Quickcheck.test] (to aid debugging).   *)

module Mica = struct
  type expr =
    (* T *)
    | Empty
    | Add of char * expr
    | Remove of char * expr
    | Union of expr * expr
    | Inter of expr * expr
    | Diff of expr * expr
    (* Bool *)
    | Disjoint of expr * expr
    | Is_empty of expr
    (* Int *)
    | Cardinal of expr
    (* CharList *)
    | Elements of expr
    (* CharOption *)
    | Min_elt_opt of expr
    | Max_elt_opt of expr
    | Choose_opt of expr
  [@@deriving show { with_path = false }, sexp_of]

  type ty = Bool | Int | CharOption | CharList | T

  let rec gen_expr (ty : ty) : expr Generator.t =
    let open Let_syntax in
    let%bind k = size in
    match (ty, k) with
    | T, 0 -> return Empty
    | Int, _ ->
      let%bind e = with_size ~size:(k / 2) (gen_expr T) in
      return @@ Cardinal e
    | Bool, _ ->
      let gen_disjoint =
        let%bind e1 = with_size ~size:(k / 2) (gen_expr T) in
        let%bind e2 = with_size ~size:(k / 2) (gen_expr T) in
        return (Disjoint (e1, e2)) in
      let gen_is_empty =
        let%bind e = with_size ~size:(k / 2) (gen_expr T) in
        return (Is_empty e) in
      union [ gen_disjoint; gen_is_empty ]
    | CharList, _ ->
      let%bind e = with_size ~size:(k / 2) (gen_expr T) in
      return @@ Elements e
    | CharOption, _ ->
      let%bind e = with_size ~size:(k / 2) (gen_expr T) in
      union [ return (Min_elt_opt e); return (Max_elt_opt e) ]
    | T, _ ->
      let gen_add =
        let%bind x1 = char_alpha in
        let%bind e2 = with_size ~size:(k / 2) (gen_expr T) in
        return @@ Add (x1, e2) in
      let gen_remove =
        let%bind x1 = char_alpha in
        let%bind e2 = with_size ~size:(k / 2) (gen_expr T) in
        return @@ Remove (x1, e2) in
      let gen_union =
        let%bind e1 = with_size ~size:(k / 2) (gen_expr T) in
        let%bind e2 = with_size ~size:(k / 2) (gen_expr T) in
        return @@ Union (e1, e2) in
      let gen_inter =
        let%bind e1 = with_size ~size:(k / 2) (gen_expr T) in
        let%bind e2 = with_size ~size:(k / 2) (gen_expr T) in
        return @@ Inter (e1, e2) in
      union [ gen_add; gen_remove; gen_union; gen_inter ]

  module Interpret (M : S) = struct
    type value =
      | ValBool of bool
      | ValInt of int
      | ValCharOption of char option
      | ValCharList of char list
      | ValT of M.t

    let rec interp (expr : expr) : value =
      match expr with
      | Empty -> ValT M.empty
      | Add (c, e) -> (
        match interp e with
        | ValT s -> ValT (M.add c s)
        | _ -> failwith "impossible")
      | Remove (c, e) -> (
        match interp e with
        | ValT s -> ValT (M.remove c s)
        | _ -> failwith "impossible")
      | Union (e1, e2) -> (
        match (interp e1, interp e2) with
        | ValT s1, ValT s2 -> ValT (M.union s1 s2)
        | _ -> failwith "impossible")
      | Inter (e1, e2) -> (
        match (interp e1, interp e2) with
        | ValT s1, ValT s2 -> ValT (M.inter s1 s2)
        | _ -> failwith "impossible")
      | Diff (e1, e2) -> (
        match (interp e1, interp e2) with
        | ValT s1, ValT s2 -> ValT (M.diff s1 s2)
        | _ -> failwith "impossible")
      | Disjoint (e1, e2) -> (
        match (interp e1, interp e2) with
        | ValT s1, ValT s2 -> ValBool (M.disjoint s1 s2)
        | _ -> failwith "impossible")
      | Is_empty e -> (
        match interp e with
        | ValT s -> ValBool (M.is_empty s)
        | _ -> failwith "impossible")
      | Cardinal e -> (
        match interp e with
        | ValT s -> ValInt (M.cardinal s)
        | _ -> failwith "impossible")
      | Elements e -> (
        match interp e with
        | ValT s -> ValCharList (M.elements s)
        | _ -> failwith "impossible")
      | Min_elt_opt e -> (
        match interp e with
        | ValT s -> ValCharOption (M.min_elt_opt s)
        | _ -> failwith "impossible")
      | Max_elt_opt e -> (
        match interp e with
        | ValT s -> ValCharOption (M.max_elt_opt s)
        | _ -> failwith "impossible")
      | Choose_opt e -> (
        match interp e with
        | ValT s -> ValCharOption (M.choose_opt s)
        | _ -> failwith "impossible")
  end

  module TestHarness (M1 : S) (M2 : S) = struct
    module I1 = Interpret (M1)
    module I2 = Interpret (M2)
    open Core

    let test_bool () =
      Quickcheck.test (gen_expr Bool) ~sexp_of:sexp_of_expr ~f:(fun e ->
        match (I1.interp e, I2.interp e) with
        | ValBool bool__041_, ValBool bool__040_ ->
          [%test_eq: bool] bool__041_ bool__040_
        | _ -> failwith "impossible")

    let test_int () =
      Quickcheck.test (gen_expr Int) ~sexp_of:sexp_of_expr ~f:(fun e ->
        match (I1.interp e, I2.interp e) with
        | ValInt int__043_, ValInt int__042_ ->
          [%test_eq: int] int__043_ int__042_
        | _ -> failwith "impossible")

    let test_char_option () =
      Quickcheck.test (gen_expr CharOption) ~sexp_of:sexp_of_expr ~f:(fun e ->
        match (I1.interp e, I2.interp e) with
        | ValCharOption char_option__047, ValCharOption char_option__046 ->
          [%test_eq: char option] char_option__047 char_option__046
        | _ -> failwith "impossible")

    let test_char_list () =
      Quickcheck.test (gen_expr CharList) ~sexp_of:sexp_of_expr ~f:(fun e ->
        match (I1.interp e, I2.interp e) with
        | ValCharList char_list__049, ValCharList char_list__048 ->
          [%test_eq: char list] char_list__049 char_list__048
        | _ -> failwith "impossible")

    let run_tests () =
      test_bool ();
      test_int ();
      test_char_option ();
      test_char_list ();
      printf "Mica: OK, passed %d observational equivalence tests.\n" 40000
  end
end

module T = Mica.TestHarness (Stdlib_Charset) (Yallop_Charset)

let () = T.run_tests ()
