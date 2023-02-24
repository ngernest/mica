open! Core
open Core.Poly
open Core.Quickcheck

module G = Generator

module QConf = struct
  type cmd =
    | Pop         (* may throw exception *)
    | Top         (* may throw exception *)
    | Push of int
    | Clear
    | Is_empty [@@deriving sexp_of, quickcheck]
    
  type state = int list
  type sut = int Queue.t

  let gen_cmd s =
    let int_gen = Int.gen_incl Int.min_value Int.max_value in
    if s = []
    then G.union  (* don't generate pop/tops from empty *)
           [G.map ~f:(fun i -> Push i) int_gen;
            G.return Clear;
            G.return Is_empty]
    else G.union (* weight the below for fewer pushes? *)
           [G.return Pop;
            G.return Top;
            G.map ~f:(fun i -> Push i) int_gen;
            G.return Clear;
            G.return Is_empty]

  let arb_cmd s =
    let shrink c = match c with
      | Push i   -> Iter.map (fun i' -> Push i') (Shrink.int i)
      | Pop
      | Top
      | Clear
      | Is_empty -> Iter.empty in
    QCheck.make ~print:show_cmd ~shrink:shrink (gen_cmd s)

  let init_state = []
  let next_state c s = match c with
    | Pop      -> (match s with
 	            | []    -> failwith "tried to pop empty queue"
		    | _::s' -> s')
    | Push i   -> (* s@[i] *)
       if i<>135 then s@[i] else s  (* an artificial fault in the model *)
    | Clear    -> []
    | Top      -> s
    | Is_empty -> s

  
  let init_sut = Queue.create ~capacity:42
  
  let cleanup _  = ()
  let run_cmd c s q = match c with
    | Pop      -> (try (Queue.pop q = List.hd s) with _ -> false)
    | Top      -> (try (Queue.top q = List.hd s) with _ -> false)
    | Push n   -> (Queue.push n q; true)
    | Clear    -> (Queue.clear q; true)
    | Is_empty -> (Queue.is_empty q) = (s = [])

  let precond c s = match c with
    | Pop -> s<>[]
    | Top -> s<>[]
    | _   -> true
end

module QT = TestHarness.Make(QConf);;

let%test_unit "consistency" = QT.consistency_test ~trials:1000;;
let%test_unit "agreement" = QT.agree_test ~trials:1000;;