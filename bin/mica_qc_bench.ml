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
  let extend _ = [ "-silent" ]

  (** [extendWithFlag flag] is like [extend] above, but this function also 
        passes [flag] to the main Mica executable when running benchmarks. *)
  let extendWithFlag flag = [ "-silent"; flag ]

  let setArgs =
    [ "./bin/main.exe";
      "./lib/sets/SetInterface.ml";
      "./lib/sets/ListSet.ml";
      "./lib/sets/BSTSet.ml"
    ]

  let stackArgs =
    [ "./bin/main.exe";
      "./lib/stacks/StackInterface.ml";
      "./lib/stacks/ListStack.ml";
      "./lib/stacks/VariantStack.ml"
    ]

  let polyArgs =
    [ "./bin/main.exe";
      "./lib/polynomials/PolyInterface.ml";
      "./lib/polynomials/Poly1.ml";
      "./lib/polynomials/Poly2.ml"
    ]

  let mapArgs =
    [ "./bin/main.exe";
      "./lib/maps/MapInterface.ml";
      "./lib/maps/AssocListMap.ml";
      "./lib/maps/RedBlackMap.ml"
    ]

  let regexArgs =
    [ "./bin/main.exe";
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

(** Launches an executable which benchmarks Mica's code generation functionality
    on the five example modules, along with the corresponding auto-generated
    PBT executables *)
let () =
  let open BenchmarkParams in
  Command_unix.run
  @@ make_command
       [ Test.create_group ~name:"Sets"
           [ Test.create ~name:"Code generation" (fun () ->
               Command_unix.run ~argv:setArgs ~extend cmdLineParser);
             Test.create ~name:"PBT executable" (fun () ->
               Command.exec ~summary
                 ~path_to_exe:(`Relative_to_me setExecutable))
           ];
         Test.create_group ~name:"Stacks"
           [ Test.create ~name:"Code generation" (fun () ->
               Command_unix.run ~argv:stackArgs ~extend cmdLineParser);
             Test.create ~name:"PBT executable" (fun () ->
               Command.exec ~summary
                 ~path_to_exe:(`Relative_to_me stackExecutable))
           ];
         Test.create_group ~name:"Polynomials"
           [ Test.create ~name:"Code generation" (fun () ->
               Command_unix.run ~argv:polyArgs
                 ~extend:(fun _ -> [ "-silent"; "-non-negative-ints-only" ])
                 cmdLineParser);
             Test.create ~name:"PBT executable" (fun () ->
               Command.exec ~summary
                 ~path_to_exe:(`Relative_to_me polyExecutable))
           ];
         Test.create_group ~name:"Maps"
           [ Test.create ~name:"Code generation" (fun () ->
               Command_unix.run ~argv:mapArgs ~extend cmdLineParser);
             Test.create ~name:"PBT executable" (fun () ->
               Command.exec ~summary
                 ~path_to_exe:(`Relative_to_me mapExecutable))
           ];
         Test.create_group ~name:"Regexes"
           [ Test.create ~name:"Code generation" (fun () ->
               Command_unix.run ~argv:regexArgs ~extend cmdLineParser);
             Test.create ~name:"PBT executable" (fun () ->
               Command.exec ~summary
                 ~path_to_exe:(`Relative_to_me regexExecutable))
           ]
       ]
