# Mica: Automated Differential Testing for OCaml Modules 

(Submission for the [ICFP '23 Student Research Competition](https://icfp23.sigplan.org/track/icfp-2023-student-research-competition))

**Mica** checks whether two OCaml modules implementing the same signature are observationally 
equivalent. Mica does this by parsing the signature & *automatically* generating 
property-based testing code specialised to the signature.

Documentation: https://ngernest.github.io/mica/mica/index.html
        
## Description of source files
- `lib/Parser.ml`: parser utility functions, modified from the Angstrom parser-combinator library
- `lib/ParserTypes.ml`: Datatypes defining an AST for module signatures
- `lib/ModuleParser.ml`: Parser for OCaml module signatures
- `lib/CodeGenerator.ml`: Takes a parsed AST representing a module signature and generates the appropriate PBT code 
- `lib/CmdLineParser.ml`: Parses user input from the command line
- `bin/main.ml`: Entry point for the Mica executable

## Examples 
The `lib` directory also contains five example module signatures, 
each of which has two different implementations. Mica has been tested
extensively with these five pairs of modules. 

**1. Finite Sets** (Lists & BSTs)
  - Signature: [`SetInterface.ml`](./lib/sets/SetInterface.ml)
  - List implementation: [`ListSet.ml`](./lib/sets/ListSet.ml)
  - BST implementation: [`BSTSet.ml`](./lib/sets/BSTSet.ml)

**2. Regex matchers** (Brzozowski derivatives & DFAs)
  - Signature: [`RegexMatcher.mli`](./lib/regexes/RegexMatcher.mli) (Commented version: [`RegexMatcher.ml`](./lib/regexes/RegexMatcher.ml))
  - Brzozowski derivatives: [`Brzozowski.ml`](./lib/regexes/Brzozowski.ml)
  - DFAs: [`DFA.ml`](./lib/regexes/DFA.ml)

**3. Polynomials** (Horner schema & list folds)
  - Signature: [`PolyInterface.ml`](./lib/polynomials/PolyInterface.ml)
  - Polynomial evaluation using list folds: [`Poly1.ml`](./lib/polynomials/Poly1.ml) (adapted from Jean-Christophe Filliatre)
  - Polynomial evaluation using Horner's algorithm: [`Poly2.ml`]((./lib/polynomials/Poly2.ml)) (adapted from Shayne Fletcher)

**4. Functional Maps** (Association lists & Red-black trees)
  - Signature: [`MapInterface.ml`](./lib/maps/MapInterface.ml) (Commented version: [`MapInterface.mli`](./lib/maps/MapInterface.mli))
  - Association lists: [`AssocListMap.ml`](./lib/maps/AssocListMap.ml)
    - See [`AssocList.ml`](./lib/maps/AssocList.ml) for the definition of the `AssocList` type & its QuickCheck generator
  - Red-Black tree maps: [`RedBlackMap.ml`](./lib/maps/RedBlackMap.ml)

**5. Stacks** (List & algebraic data type representations)
  - Signature: `StackInterface.ml`
  - Implementation using lists: `ListStack.ml`
  - Implementation using custom ADTs: `VariantStack.ml`

## Building & running
Run `make` (or `dune build`) to build and compile the library.         
Run `make install` to install dependencies. 

**Usage**:       
```
dune exec -- bin/main.exe [.ml file containing signature] [.ml file containing 1st module] [.ml file containing 2nd module]
```
This command runs Mica and creates two new files:
1. `lib/Generated.ml` (contains PBT code for testing an OCaml module)
2. `bin/compare_impls.ml` (code for an executable that tests two modules for observational equivalence)

To run the generated executable, run `dune exec -- bin/compare_impls.exe`. 

### Example (finite sets)
```
  dune exec -- bin/main.exe lib/sets/SetInterface.ml lib/sets/ListSet.ml lib/sets/BSTSet.ml
```
This runs Mica on the Set example above, checking if the `ListSet` and `BSTSet` modules 
both correctly implement the interface `SetInterface`.       
The files `lib/GeneratedSetPBTCode.ml` and `lib/GeneratedSetExecutable.ml` contain PBT code that is 
automatically generated by Mica. 

### Dependencies:
- [Base](https://github.com/janestreet/base)
- [Core](https://github.com/janestreet/core)
- [Angstrom](https://github.com/inhabitedtype/angstrom)
- [PPrint](https://github.com/fpottier/pprint)
- [Stdio](https://github.com/janestreet/stdio)

## Acknowledgements
I'm incredibly fortunate to have two fantastic advisors: [Harry Goldstein](https://harrisongoldste.in) & Prof. [Benjamin Pierce](https://www.cis.upenn.edu/~bcpierce/). Harry & Benjamin have 
provided lots of useful feedback on the design of Mica, and I'm grateful for 
their mentorship.

I'd also like to thank:
- Penn's [PLClub](https://www.cis.upenn.edu/~plclub/) for their feedback on my presentation slides
- [Jan Midtggard](http://janmidtgaard.dk) (Tarides) & Carl Eastlund (Jane Street) for answering 
my questions about QCSTM and Core.Quickcheck
- [Cassia Torczon](https://cassiatorczon.github.io) for the idea behind the name Mica


## Origin of name
Mica stands for "Module-Implementation Comparison Automation". Mica's name, a type of 
[mineral](https://en.wikipedia.org/wiki/Mica) commonly found in rocks, was inspired 
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
The implementation of red-black tree deletion follows the approach taken by [Germane & Might (2014)](https://matt.might.net/papers/germane2014deletion.pdf).

Stacks:
- [Cornell CS 3110 textbook, chapter 5](https://cs3110.github.io/textbook/chapters/modules/functional_data_structures.html#stacks)
