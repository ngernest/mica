open Base
open Base_quickcheck
open SetInterface

(******************************************************************************)
(** The following is very similar to the code that Mica would generate
   automatically. The only manual modifications are: 
   - Variable renaming
   - We use [base_quickcheck] instead of [Core.Quickcheck] as the underlying
   PBT library (only the former has an option to continue running tests 
   even after the property is falsified)
   - We include extra case statements to handle exceptions thrown by students'
   code (e.g. if the student put [failwith ...] in their code) *)

module Mica = struct
  type expr =
    | Empty
    | Is_empty of expr
    | Member of int * expr
    | Add of int * expr
    | Remove of int * expr
    | Size of expr
    | List_of_set of expr
    | Set_of_list of int list [@sexp.list]
    | Equals of expr * expr
  [@@deriving sexp_of, quickcheck]

  type ty = Bool | Int | IntList | IntT

  module Interpret (M : SET) = struct
    type value =
      | ValBool of bool
      | ValInt of int
      | ValIntList of int list
      | ValIntT of int M.set

    let rec interp (expr : expr) : value =
      match expr with
      | Empty -> ValIntT M.empty
      | Is_empty e -> (
        match interp e with
        | ValIntT e' -> ValBool (M.is_empty e')
        | _ -> failwith "impossible")
      | Member (x1, e2) -> (
        match interp e2 with
        | ValIntT e' -> ValBool (M.member x1 e')
        | _ -> failwith "impossible")
      | Add (x1, e2) -> (
        match interp e2 with
        | ValIntT e' -> ValIntT (M.add x1 e')
        | _ -> failwith "impossible")
      | Remove (x1, e2) -> (
        match interp e2 with
        | ValIntT e' -> ValIntT (M.remove x1 e')
        | _ -> failwith "impossible")
      | Size e -> (
        match interp e with
        | ValIntT e' -> ValInt (M.size e')
        | _ -> failwith "impossible")
      | List_of_set e -> (
        match interp e with
        | ValIntT e' -> ValIntList (M.list_of_set e')
        | _ -> failwith "impossible")
      | Set_of_list xs -> ValIntT (M.set_of_list xs)
      | Equals (e1, e2) -> (
        match (interp e1, interp e2) with
        | ValIntT e1', ValIntT e2' -> ValBool (M.equals e1' e2')
        | _ -> failwith "impossible")
  end

  let rec gen_expr (ty : ty) : expr Generator.t =
    let module G = Generator in
    let open G.Let_syntax in
    let%bind k = G.size in
    match (ty, k) with
    | IntT, 0 -> return Empty
    | Bool, _ ->
      let is_empty =
        let%bind e = G.with_size ~size:(k / 2) (gen_expr IntT) in
        G.return @@ Is_empty e in
      let member =
        let%bind x1 = G.small_positive_or_zero_int in
        let%bind e2 = G.with_size ~size:(k / 2) (gen_expr IntT) in
        G.return @@ Member (x1, e2) in
      let equals =
        let%bind e1 = G.with_size ~size:(k / 2) (gen_expr IntT) in
        let%bind e2 = G.with_size ~size:(k / 2) (gen_expr IntT) in
        G.return @@ Equals (e1, e2) in
      G.union [ is_empty; member; equals ]
    | Int, _ ->
      let size =
        let%bind e = G.with_size ~size:(k / 2) (gen_expr IntT) in
        G.return @@ Size e in
      size
    | IntList, _ ->
      let list_of_set =
        let%bind e = G.with_size ~size:(k / 2) (gen_expr IntT) in
        G.return @@ List_of_set e in
      list_of_set
    | IntT, _ ->
      let add =
        let%bind x1 = G.small_positive_or_zero_int in
        let%bind e2 = G.with_size ~size:(k / 2) (gen_expr IntT) in
        G.return @@ Add (x1, e2) in
      let remove =
        let%bind x1 = G.small_positive_or_zero_int in
        let%bind e2 = G.with_size ~size:(k / 2) (gen_expr IntT) in
        G.return @@ Remove (x1, e2) in
      let set_of_list =
        let%bind xs = G.list G.small_positive_or_zero_int in
        G.return @@ Set_of_list xs in
      G.union [ add; remove; set_of_list ]

  (** Helper functor: creates the [Test.S] module needed for [base_quickcheck]
    with the [quickcheck_generator] specialized to produce [expr]s 
    of a certain [ty] *)
  module Make_S (G : sig
    val gen_typed_expr : expr Generator.t
  end) =
  struct
    type t = expr

    let sexp_of_t = sexp_of_expr
    let quickcheck_generator = G.gen_typed_expr
    let quickcheck_shrinker = quickcheck_shrinker_expr
  end

  let string_of_list (xs : int list) : string =
    Printf.sprintf "[%s]"
      (String.concat ~sep:"; " (List.map ~f:Int.to_string xs))

  let incr (r : int ref) : unit = r := !r + 1

  module TestHarness (M1 : SET) (M2 : SET) = struct
    module I1 = Interpret (M1)
    module I2 = Interpret (M2)
    open Base_quickcheck

    let sexp_of = sexp_of_expr
    let seed : Test.Config.Seed.t = Nondeterministic
    let test_count = 1000
    let config : Test.Config.t = { Test.default_config with seed; test_count }

    let test_bool =
      let bool_count = ref 0 in
      let module S = Make_S (struct
        let gen_typed_expr = gen_expr Bool
      end) in
      Test.result
        (module S)
        ~config
        ~f:(fun e ->
          incr bool_count;
          match (I1.interp e, I2.interp e) with
          | ValBool b1, ValBool b2 ->
            if Bool.equal b1 b2 then Ok ()
            else
              let s1, s2 =
                ( String.lowercase (Bool.to_string b1),
                  String.lowercase (Bool.to_string b2) ) in
              Error ("bool", s1, s2, !bool_count)
          | exception _ -> Error ("bool_exn", "exn", "exn", !bool_count)
          | _ -> failwith "failed bool")

    let test_int =
      let int_count = ref 0 in
      let module S = Make_S (struct
        let gen_typed_expr = gen_expr Int
      end) in
      Test.result
        (module S)
        ~config
        ~f:(fun e ->
          incr int_count;
          match (I1.interp e, I2.interp e) with
          | ValInt i1, ValInt i2 ->
            if Int.equal i1 i2 then Ok ()
            else
              let s1, s2 = (Int.to_string i1, Int.to_string i2) in
              Error ("int", s1, s2, !int_count)
          | exception _ -> Error ("int_exn", "exn", "exn", !int_count)
          | _ -> failwith "failed int")

    let test_int_list =
      let int_list_count = ref 0 in
      let module S = Make_S (struct
        let gen_typed_expr = gen_expr IntList
      end) in
      Test.result
        (module S)
        ~config
        ~f:(fun e ->
          incr int_list_count;
          match (I1.interp e, I2.interp e) with
          | ValIntList l1, ValIntList l2 ->
            if List.equal Int.equal l1 l2 then Ok ()
            else
              let s1, s2 = (string_of_list l1, string_of_list l2) in
              Error ("int list", s1, s2, !int_list_count)
          | exception _ -> Error ("int_list_exn", "exn", "exn", !int_list_count)
          | _ -> failwith "failed int")

    let run_tests (i : int) (chan : Stdio.Out_channel.t) : unit =
      let open Stdio in
      let res =
        Result.combine_errors_unit [ test_bool; test_int; test_int_list ] in
      match res with
      | Ok _ -> ()
      | Error errs ->
        List.iter errs ~f:(fun (e, (ty, v1, v2, num_trials)) ->
            Out_channel.fprintf chan "%d,%s,%s,%s,%s,%d\n" i ty
              (Sexp.to_string (sexp_of_expr e))
              v1 v2 num_trials)
  end
end
