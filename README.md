# Mica: Automated Differential Testing for OCaml Modules 

[![view - Documentation](https://img.shields.io/badge/view-Documentation-blue?style=for-the-badge)](https://ngernest.github.io/mica/mica/index.html)

## Update (May 2024)
We are currently working on re-implementing Mica as a PPX compiler extension, 
and we aim to have the PPX implementation ready for the OCaml Workshop 2024. 

This repo contains the artifact for a Mica prototype presented at the ICFP '23 SRC. 
This repo will be updated with the PPX implementation when it is ready.

***

## Overview 

**Mica** checks whether two OCaml modules implementing the same signature are observationally 
equivalent. Mica does this by parsing the signature & *automatically* generating 
property-based testing (PBT) code specialised to the signature. 

Received 2nd prize at the [ICFP '23 Student Research Competition](https://icfp23.sigplan.org/track/icfp-2023-student-research-competition).

ICFP SRC documents:              
- [Extended Abstract](./talks/icfp_src_abstract.pdf) 
- [Poster](./talks/icfp_src_poster.pdf)
- [Talk](./talks/mica_icfp_talk.pdf) 

Other talks on Mica:                 
- [Penn PLClub talk](./talks/mica_plclub_talk.pdf) (~45 mins) 
- [OPLSS '23 talk](./talks/mica_oplss_slides.pdf) (~10 mins)
        
## Description of source files 
Core components of Mica:
- [`Parser.ml`](./lib/Parser.ml): parser utility functions, modified from the Angstrom parser-combinator library
- [`ParserTypes.ml`](./lib/ParserTypes.ml): Datatypes defining an AST for OCaml module signatures
- [`ModuleParser.ml`](./lib/ModuleParser.ml): Parser for OCaml module signatures
- [`CodeGenerator.ml`](./lib/CodeGenerator.ml): Takes a parsed AST representing a module signature & generates specialized PBT code 
- [`CmdLineParser.ml`](./lib/CmdLineParser.ml): Parses user input from the command line

Auxiliary utility files:
- [`Utils.ml`](./lib/Utils.ml): Utility functions for the code generator
- [`Stats.ml`](./lib/Stats.ml): Functions for examining the distribution of randomly generated symbolic expressions
- [`Latin.ml`](./lib/Latin.ml): QuickCheck generator for Latin words (taken from [Lorem Ipsum](https://en.wikipedia.org/wiki/Lorem_ipsum) placeholder text)

Benchmark suites: 
- [`Mica_QC_Bench.ml`](./bin/mica_qc_bench.ml): Benchmarks Mica's parser, code generator & the auto-generated PBT executables
- [`Mica_Bench.ml`](./bin/mica_bench.ml): Benchmarks Mica's parser & code generator only 

## Examples 
The [`lib`](./lib) directory also contains five example module signatures, 
each of which has two different implementations. Mica has been tested
extensively with these five pairs of modules. 

The code for these example modules has been adapted from various sources, 
listed in the [references](#references) section of this README.

**1. Finite Sets** (Lists & BSTs)
  - Signature: [`SetInterface.ml`](./lib/sets/SetInterface.ml)
  - List implementation: [`ListSet.ml`](./lib/sets/ListSet.ml)
  - BST implementation: [`BSTSet.ml`](./lib/sets/BSTSet.ml)
  - Running the auto-generated code: `dune exec -- sets`
    - Example auto-generated code: [`GeneratedSetPBTCode.ml`](./lib/sets/GeneratedSetPBTCode.ml) & 
      [`GeneratedSetExecutable.ml`](./bin/GeneratedSetExecutable.ml)

**2. Regex matchers** ([Brzozowski derivatives](https://en.wikipedia.org/wiki/Brzozowski_derivative) & DFAs)
  - Signature: [`RegexMatcher.mli`](./lib/regexes/RegexMatcher.mli) (Commented version: [`RegexMatcher.ml`](./lib/regexes/RegexMatcher.ml))
  - Brzozowski derivatives: [`Brzozowski.ml`](./lib/regexes/Brzozowski.ml)
  - DFAs: [`DFA.ml`](./lib/regexes/DFA.ml)
  - Running the auto-generated code: `dune exec -- regexes`
    - Example auto-generated code: [`GeneratedRegexPBTCode.ml`](./lib/regexes/GeneratedRegexPBTCode.ml) & 
      [`GeneratedRegexExecutable.ml`](./bin/GeneratedRegexExecutable.ml)

**3. Polynomials** ([Horner schema](https://en.wikipedia.org/wiki/Horner%27s_method) & list folds)
  - Signature: [`PolyInterface.ml`](./lib/polynomials/PolyInterface.ml)
  - Polynomial evaluation using list folds: [`Poly1.ml`](./lib/polynomials/Poly1.ml) 
  - Polynomial evaluation using Horner's algorithm: [`Poly2.ml`]((./lib/polynomials/Poly2.ml))
  - Running the auto-generated code: `dune exec -- polynomials`
    - Example auto-generated code: [`GeneratedPolyPBTCode.ml`](./lib/polynomials/GeneratedPolyPBTCode.ml) &
      [`GeneratedPolyExecutable.ml`](./bin/GeneratedPolyExecutable.ml)

**4. Functional Maps** (Association lists & Red-black trees)
  - Signature: [`MapInterface.ml`](./lib/maps/MapInterface.ml) (Commented version: [`MapInterface.mli`](./lib/maps/MapInterface.mli))
  - Association lists: [`AssocListMap.ml`](./lib/maps/AssocListMap.ml)
    - See [`AssocList.ml`](./lib/maps/AssocList.ml) for the definition of the `AssocList` type & its QuickCheck generator
  - Red-Black tree maps: [`RedBlackMap.ml`](./lib/maps/RedBlackMap.ml)
  - Running the auto-generated code: `dune exec -- maps`
    - Example auto-generated code: [`GeneratedMapPBTCode.ml`](./lib/maps/GeneratedMapPBTCode.ml) &
      [`GeneratedMapExecutable.ml`](./bin/GeneratedMapExecutable.ml)

**5. Stacks** (List & algebraic data type representations)
  - Signature: [`StackInterface.ml`](./lib/stacks/StackInterface.ml)
  - List implementation: [`ListStack.ml`](./lib/stacks/ListStack.ml)
  - Implementation using custom ADT: [`VariantStack.ml`](./lib/stacks/VariantStack.ml)
  - Running the auto-generated code: `dune exec -- stacks`
    - Example auto-generated code: [`GeneratedStackPBTCode.ml`](./lib/stacks/GeneratedStackPBTCode.ml) &
     [`GeneratedStackExecutable.ml`](./bin/GeneratedStackExecutable.ml)

## Building & running
Run `make` (or `dune build`) to build and compile the library.         
Run `make install` to install dependencies. 

**Usage**:       
```
dune exec -- mica [.ml file containing signature] [.ml file containing 1st module] [.ml file containing 2nd module]
```
This command runs Mica and creates two new files:
1. `lib/Generated.ml` (contains PBT code for testing an OCaml module)
2. `bin/compare_impls.ml` (code for an executable that tests two modules for observational equivalence)

To run the generated executable, run `dune exec -- compare_impls`. 

### Example (finite sets)
```
  dune exec -- mica lib/sets/SetInterface.ml lib/sets/ListSet.ml lib/sets/BSTSet.ml
```
This runs Mica on the Set example above, checking if the [`ListSet`](./lib/sets/ListSet.ml) and [`BSTSet`](./lib/sets/BSTSet.ml) modules 
both correctly implement the interface [`SetInterface`](./lib/sets/SetInterface.ml).       
The files [`GeneratedSetPBTCode.ml`](./lib/sets/GeneratedSetPBTCode.ml) and [`GeneratedSetExecutable.ml`](./bin/GeneratedSetExecutable.ml) contain PBT code that is 
automatically generated by Mica. 

### Performance Benchmarks
- Run `dune exec -- mica_qc_bench` to benchmark *both* Mica's code-generation executable 
& the auto-generated PBT executables on all example modules
- Run `dune exec -- mica_bench` to benchmark *only* the main Mica executable
  - When this option is selected, the auto-generated PBT executables will not be benchmarked

The following table contains performance benchmarks for Mica on the five example module signatures.       
Benchmarks were conducted on an 2023 M2 Macbook Pro using Jane Street's [Core_bench](https://github.com/janestreet/core_bench) library, 
which estimates Mica's runtime over a large no. of runs (~1000) using linear regression ([link](https://blog.janestreet.com/core_bench-micro-benchmarking-for-ocaml/)). 

| **Example Module Signature** | **Runtime of Mica Parser & Code Generator** | **Runtime of Auto-Generated PBT executable** |
| :--------------------------: | :-----------------------------------------: | :------------------------------------------: |
|             Sets             |                  309.25 μs                  |                   2.55 ns                    |
|            Stacks            |                  361.08 μs                  |                   2.54 ns                    |
|         Polynomials          |                  302.82 μs                  |                   2.57 ns                    |
|             Maps             |                  262.84 μs                  |                   2.56 ns                    |
|           Regexes            |                  266.61 μs                  |                   2.57 ns                    |



### Dependencies
Please see `mica.opam` for a full list of dependencies. 
Mica has been tested primarily with versions 4.13.1 & 5.0.0 of the OCaml base compiler (on an M1 Mac). 
- [Base](https://github.com/janestreet/base) & [Base_quickcheck](https://github.com/janestreet/base_quickcheck)
- [Core](https://github.com/janestreet/core) & [Core.Quickcheck](https://blog.janestreet.com/quickcheck-for-core/)
- [Angstrom](https://github.com/inhabitedtype/angstrom)
- [PPrint](https://github.com/fpottier/pprint)
- [Stdio](https://github.com/janestreet/stdio)
- [Re](https://github.com/ocaml/ocaml-re)
- [Odoc](https://github.com/ocaml/ocaml-re) (for generating documentation)
- [Core_bench](https://github.com/janestreet/core_bench) (for running benchmarks)
- [OCaml-CI](https://github.com/ocurrent/ocaml-ci) (for CI)

## Acknowledgements
I'm incredibly fortunate to have two fantastic advisors: [Harry Goldstein](https://harrisongoldste.in) & Prof. [Benjamin Pierce](https://www.cis.upenn.edu/~bcpierce/). Harry & Benjamin have 
provided lots of useful feedback on the design of Mica, and I'm extremely grateful for 
their mentorship. 

I'd also like to thank:
- Penn's [PLClub](https://www.cis.upenn.edu/~plclub/) for their feedback on my presentation slides
- [Jan Midtggard](http://janmidtgaard.dk) (Tarides) & Carl Eastlund (Jane Street) for answering 
my questions about QCSTM and Core.Quickcheck
- [Cassia Torczon](https://cassiatorczon.github.io) for the idea behind the name Mica


## Origin of name
Mica stands for "Module-Implementation Comparison Automation". Mica is also a type
[mineral](https://en.wikipedia.org/wiki/Mica) commonly found in rocks -- this name was inspired 
by several relevant OCaml libraries that are also named after stones/rocks:         
- [Monolith](https://gitlab.inria.fr/fpottier/monolith) (Randomised testing tool for OCaml modules)
- [Opal](https://github.com/pyrocat101/opal) (parser combinator library)
- [Menhir](http://gallium.inria.fr/~fpottier/menhir/) (parser library)
- [Obelisk](https://github.com/Lelio-Brun/Obelisk) (pretty-printer for Menhir)

## References
- Jane Street's [Base_quickcheck](https://opensource.janestreet.com/base_quickcheck/) & [Core.Quickcheck](https://blog.janestreet.com/quickcheck-for-core/)
- [QuviQ QuickCheck](https://dl.acm.org/doi/10.1145/1159789.1159792)
- [QCSTM](https://github.com/jmid/qcstm)   
- [Model_quickcheck](https://github.com/suttonshire/model_quickcheck)
- [Monolith](https://gitlab.inria.fr/fpottier/monolith)
- [Articheck](http://www.lix.polytechnique.fr/Labo/Gabriel.Scherer/doc/articheck-long.pdf)
- [*Advanced Topics in Types & Programming Languages*](https://www.cis.upenn.edu/~bcpierce/attapl/), Chapter 8
- [*Real World OCaml*](https://dev.realworldocaml.org/index.html)

The code for the example modules has been adapted from the following sources:

Finite Sets:
- [Cornell CS 3110 OCaml textbook](https://cs3110.github.io/textbook/chapters/ds/hash_tables.html#maps-as-hash-tables)
- [Penn CIS 1200 lecture notes](https://www.seas.upenn.edu/~cis120/23su/files/120notes.pdf#page=3)
- [Yale-NUS YSC2229 website](https://ilyasergey.net/YSC2229/week-11-bst.html)         

Regex matchers:
- Harry Goldstein's [livestream](https://www.youtube.com/watch?v=QaMU0wMMczU&t=2199s) on regex derivatives & DFAs

Polynomials:
- [Jean-Christophe Filliatre's implementation](https://www.lri.fr/~filliatr/ftp/ocaml/ds/poly.ml.html)
- [Shayne Fletcher's implementation](https://blog.shaynefletcher.org/2017/03/polynomials-over-rings.html)

Functional maps:  
- [Colin Shaw's RB tree implementation](https://github.com/CompScienceClub/ocaml-red-black-trees)
- [Benedikt Meurer RB tree implementation](https://github.com/bmeurer/ocaml-rbtrees/blob/master/src/rbset.ml)
- [Cornell CS 3110 textbook, chapter 8](https://cs3110.github.io/textbook/chapters/ds/rb.html#id1)            
- The implementation of red-black tree deletion follows the approach taken by 
[Germane & Might (2014)](https://matt.might.net/papers/germane2014deletion.pdf).

Stacks:
- [Cornell CS 3110 textbook, chapter 5](https://cs3110.github.io/textbook/chapters/modules/functional_data_structures.html#stacks)
