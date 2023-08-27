open Core 
open Core_bench
open Lib.CmdLineParser

let () = 
  Command_unix.run @@ Bench.make_command [
    Bench.Test.create ~name:"Sets" (fun () -> 
      Command_unix.run ~argv:[
        "./bin/main.exe";
        "./lib/sets/SetInterface.ml"; 
        "./lib/sets/ListSet.ml";
        "./lib/sets/BSTSet.ml"
      ] ~extend:(fun _ -> ["-silent"]) cmdLineParser
    );

    Bench.Test.create ~name:"Stacks" (fun () -> 
      Command_unix.run ~argv:[
        "./bin/main.exe";
        "./lib/stacks/StackInterface.ml"; 
        "./lib/stacks/ListStack.ml";
        "./lib/stacks/VariantStack.ml"
      ] ~extend:(fun _ -> ["-silent"]) cmdLineParser);
    
    Bench.Test.create ~name:"Polynomials" (fun () -> 
      Command_unix.run ~argv:[
        "./bin/main.exe";
        "./lib/polynomials/PolyInterface.ml"; 
        "./lib/polynomials/Poly1.ml";
        "./lib/polynomials/Poly2.ml"
      ] ~extend:(fun _ -> ["-silent"; "-non-negative-ints-only"]) cmdLineParser);

    Bench.Test.create ~name:"Maps" (fun () -> 
      Command_unix.run ~argv:[
        "./bin/main.exe";
        "./lib/maps/MapInterface.ml"; 
        "./lib/maps/AssocListMap.ml";
        "./lib/maps/RedBlackMap.ml"
      ] ~extend:(fun _ -> ["-silent"]) cmdLineParser
    );

    Bench.Test.create ~name:"Regexes" (fun () -> 
      Command_unix.run ~argv:[
        "./bin/main.exe";
        "./lib/regexes/RegexMatcher.mli"; 
        "./lib/regexes/Brzozowski.ml";
        "./lib/regexes/DFA.ml"
      ] ~extend:(fun _ -> ["-silent"]) cmdLineParser
    );
  ]