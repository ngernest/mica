open Core_bench.Bench
open Core
open Lib.CmdLineParser

(** Benchmarks only Mica's parser & code generator. The benchmarks 
    are run on all five example modules in the [lib] directory.  
    (Sets, Regexes, Polynomials, Maps, Stacks). *)

(** Launches an executable which benchmarks Mica's code generation functionality
    on the five example modules *)
let () =
  Command_unix.run
  @@ make_command
       [ Test.create ~name:"Sets" (fun () ->
           Command_unix.run
             ~argv:
               [ "./bin/main.exe";
                 "./lib/sets/SetInterface.ml";
                 "./lib/sets/ListSet.ml";
                 "./lib/sets/BSTSet.ml"
               ]
             ~extend:(fun _ -> [ "-silent" ])
             cmdLineParser);
         Test.create ~name:"Stacks" (fun () ->
           Command_unix.run
             ~argv:
               [ "./bin/main.exe";
                 "./lib/stacks/StackInterface.ml";
                 "./lib/stacks/ListStack.ml";
                 "./lib/stacks/VariantStack.ml"
               ]
             ~extend:(fun _ -> [ "-silent" ])
             cmdLineParser);
         Test.create ~name:"Polynomials" (fun () ->
           Command_unix.run
             ~argv:
               [ "./bin/main.exe";
                 "./lib/polynomials/PolyInterface.ml";
                 "./lib/polynomials/Poly1.ml";
                 "./lib/polynomials/Poly2.ml"
               ]
             ~extend:(fun _ -> [ "-silent"; "-non-negative-ints-only" ])
             cmdLineParser);
         Test.create ~name:"Maps" (fun () ->
           Command_unix.run
             ~argv:
               [ "./bin/main.exe";
                 "./lib/maps/MapInterface.ml";
                 "./lib/maps/AssocListMap.ml";
                 "./lib/maps/RedBlackMap.ml"
               ]
             ~extend:(fun _ -> [ "-silent" ])
             cmdLineParser);
         Test.create ~name:"Regexes" (fun () ->
           Command_unix.run
             ~argv:
               [ "./bin/main.exe";
                 "./lib/regexes/RegexMatcher.mli";
                 "./lib/regexes/Brzozowski.ml";
                 "./lib/regexes/DFA.ml"
               ]
             ~extend:(fun _ -> [ "-silent" ])
             cmdLineParser)
       ]
