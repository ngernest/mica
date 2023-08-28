open Core 
open Core_bench.Bench
open Lib.CmdLineParser

(** Benchmarks Mica's parser & code generator, along with the 
    auto-generated PBT executable produced by Mica. The benchmarks 
    are run on all five example modules in the [lib] directory.  
    (Sets, Regexes, Polynomials, Maps, Stacks). *)

(** Auxiliary module containing parameters and arguments for running 
    benchmark tests, e.g. the names of relevant modules *)
module BenchmarkParams = struct 
  (** List of flags to be passed to the main Mica executable when performing
      benchmark tests (used by [Command_unix.run]). So far, we only pass the 
      [-silent] flag to Mica, which suppresses printing to [stdout]. *)
  let extend _ = ["-silent"]

  (** [extendWithFlag flag] is like [extend] above, but this function also 
      passes [flag] to the main Mica executable when running benchmarks. *)
  let extendWithFlag flag = ["-silent"; flag]

  let setArgs = [
      "./bin/main.exe";
      "./lib/sets/SetInterface.ml"; 
      "./lib/sets/ListSet.ml";
      "./lib/sets/BSTSet.ml"
    ]

  let stackArgs = [
    "./bin/main.exe";
    "./lib/stacks/StackInterface.ml"; 
    "./lib/stacks/ListStack.ml";
    "./lib/stacks/VariantStack.ml"
  ]

  let polyArgs = [
    "./bin/main.exe";
    "./lib/polynomials/PolyInterface.ml"; 
    "./lib/polynomials/Poly1.ml";
    "./lib/polynomials/Poly2.ml"
  ]

  let mapArgs = [
    "./bin/main.exe";
    "./lib/maps/MapInterface.ml"; 
    "./lib/maps/AssocListMap.ml";
    "./lib/maps/RedBlackMap.ml"
  ]

  let regexArgs = [
    "./bin/main.exe";
    "./lib/regexes/RegexMatcher.mli"; 
    "./lib/regexes/Brzozowski.ml";
    "./lib/regexes/DFA.ml"
  ]

  let setExecutable = "GeneratedSetExecutable.exe"

  let stackExecutable = "GeneratedStackExecutable.exe"

  let polyExecutable = "GeneratedPolyExecutable.exe"

  let mapExecutable = "GeneratedMapExecutable.exe"

  let regexExecutable = "GeneratedRegexExecutable.exe"

  let summary = "Auto-generated PBT executable produced by Mica"
end 


(** [mica_qc_bench name argv exe] benchmarks the Mica executable {i and}
    the auto-generated QuickCheck executable on an example module called [name]
    with arguments [argv], where [exe] is the name of the auto-generated executable *)
let mica_qc_bench ~(name : string) ~(argv : string list) 
                  ~(exe : string) : Test.t = 
  let open BenchmarkParams in 
  Test.create_group ~name [
    Test.create ~name:"Code generation" (fun () -> 
      Command_unix.run ~argv ~extend cmdLineParser);
    Test.create ~name:"PBT executable" (fun () -> 
      Command.exec ~summary ~path_to_exe:(`Relative_to_me exe))
  ]

(** [mica_bench name argv] is like [mica_qc_bench], 
    except it {i only} benchmarks the Mica executable on the example called [name] 
    with arguments [argv]. The auto-generated PBT executable is {i not}
    benchmarked.  *)  
let mica_bench ~(name : string) ~(argv : string list) : Test.t = 
  let open BenchmarkParams in 
  Test.create ~name @@ fun () -> 
    Command_unix.run ~argv ~extend cmdLineParser

let () = 
  let open BenchmarkParams in 
  Command_unix.run @@ make_command [
    mica_qc_bench ~name:"Sets" ~argv:setArgs ~exe:setExecutable;
    mica_qc_bench ~name:"Stacks" ~argv:stackArgs ~exe:stackExecutable;
    mica_qc_bench ~name:"Polynomials" ~argv:polyArgs ~exe:polyExecutable;
    mica_qc_bench ~name:"Maps" ~argv:mapArgs ~exe:mapExecutable;
    mica_qc_bench ~name:"Regexes" ~argv:regexArgs ~exe:regexExecutable
  ]

(* TODO: figure out how to set up a cmd-line flag that specifies 
   if only the mica_benchmark should run, or if mica_qc_benchmark should run *)  

(* TODO: figure out how to get Odoc to generate docs for [benchmark.ml] ? *)    