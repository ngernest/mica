open! Core
open Core.Poly
open Core.Quickcheck

module G = Generator

module HConf =
struct
  (** For simplicity, we test hashtables with string keys & int values *)
  type state = (string * int) list
  
  (** For simplicity, we test hashtables with string keys & int values *)
  type sut   = (string, int) Hashtbl.t

  type cmd =
    | Clear
    | Add of string * int
    | Remove of string
    | Find of string
    | Replace of string * int
    | Mem of string
    | Length [@@deriving sexp_of, quickcheck]

  (*  gen_cmd : state -> command Generator.t  *)
  let gen_cmd (st : state) =
    let open Base_quickcheck.Generator in 
    let small_string_gen = 
      small_positive_or_zero_int >>= fun n -> String.gen_with_length n G.char in 
    let int_gen = Int.gen_incl Int.min_value Int.max_value in
    let str_gen =
      if st = [] then small_string_gen
      else
        let keys = List.map ~f:fst st in
        G.union [
          G.of_list keys; 
          small_string_gen;
          String.gen' G.char] in
    G.union
      [ G.return Clear;
        G.map2 str_gen int_gen ~f:(fun k v -> Add (k,v));
        G.map str_gen ~f:(fun k   -> Remove k);
        G.map str_gen ~f:(fun k   -> Find k);
        G.map2 str_gen int_gen ~f:(fun k v -> Replace (k,v));
        G.map str_gen ~f:(fun k   -> Mem k);
        G.return Length; ]

  (* TODO: handle shrinking *)
  (* let shrink c = let open Iter in match c with
    | Clear      -> Iter.empty
    | Add (k,v)  ->
      (Iter.map (fun k' -> Add (k',v)) (Shrink.string k)) <+>
      (Iter.map (fun v' -> Add (k,v')) (Shrink.int v))
    | Remove k   -> Iter.map (fun k' -> Remove k') (Shrink.string k)
    | Find k     -> Iter.map (fun k' -> Find k') (Shrink.string k)
    | Replace (k,v)  ->
      (Iter.map (fun k' -> Replace (k',v)) (Shrink.string k)) <+>
      (Iter.map (fun v' -> Replace (k,v')) (Shrink.int v))
    | Mem k      -> Iter.map (fun k' -> Mem k') (Shrink.string k)
    | Length     -> Iter.empty *)

  (** Initial state for the model is just an empty list *)
  let init_state  = []

  let next_state cmd st = match cmd with
    | Clear         -> []
    | Add (k,v)     -> (k,v)::st
    | Remove k      -> List.Assoc.remove st ~equal:String.equal k 
    | Replace (k,v) -> (k,v)::(List.Assoc.remove st ~equal:String.equal k )
    | Find _
    | Mem _
    | Length        -> st

  (** Initialize a Hashtable with String keys w/ initial size 42 *)
  let init_sut () = Hashtbl.create ~size:42 (module String)
  let cleanup _   = ()
  let run_cmd cmd st table = match cmd with
    | Clear         -> Hashtbl.clear table; true
    | Add (k,v)     -> Hashtbl.add_exn table ~key:k ~data:v; true
    | Remove k      -> Hashtbl.remove table k; true
    | Find k        -> 
        (List.Assoc.find_exn st ~equal:String.equal k) = (Hashtbl.find_exn table k)
    | Replace (k,v) -> Hashtbl.set table ~key:k ~data:v; true
    | Mem k         -> 
        List.Assoc.mem st ~equal:String.equal k = Hashtbl.mem table k
    | Length        -> List.length st = Hashtbl.length table

  let precond (cmd : cmd) (st : state) = 
    match cmd with 
    | Add (k, _) -> 
        not (List.Assoc.mem st ~equal:String.equal k)
    | Remove k | Find k | Replace (k, _) -> 
        List.Assoc.mem st ~equal:String.equal k
    | _ -> true
end


module HT = TestHarness.Make(HConf);;

let%test_unit "consistency" = HT.consistency_test ~trials:1000;;
let%test_unit "agreement" = HT.agree_test ~trials:1000;;
