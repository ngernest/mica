# Automated Property-Based Testing (PBT) for OCaml Modules 

## Description of repo

### The `lib` directory
- `Parser.ml`: parser utility functions, modified from the Angstrom parser-combinator library
- `ParserTypes.ml`: Datatypes defining an AST for module signatures
- `ModuleParser.ml`: Parser for OCaml module signatures
- `CodeGenerator.ml`: Takes a parsed AST representing a module signature and generates the appropriate PBT code 
- `CmdLineParser.ml`: Parses user input from the command line

The `lib` directory also contains examples of the test harness adapted to work with the following modules (work in progress):
- Finite sets
  - Signature: `SetInterface.ml`
  - Implementation using lists: `ListSet.ml` (adapted from Yale-NUS YSC2229 website)
  - Implementation using BSTs: `BSTSet.ml` (adapted from Penn CIS 1200 lecture notes)
- Stacks
  - Signature: `StackInterface.ml`
  - Implementation using lists: `ListStack.ml` (adapted from Jane Street's Base library)
  - Implementation using custom ADTs: `VariantStack.ml` (adapted from Cornell CS 3110 textbook)
- Polynomials
  - Signature: `PolyInterface.ml`
  - Implementation using lists of tuples: `Poly1.ml` (adapted from Jean-Christophe Filliatre)
  - Implementation using lists of records: `Poly2.ml` (adapted from Shayne Fletcher)


## References
- Jane Street's [Base_quickcheck](https://opensource.janestreet.com/base_quickcheck/)
- [QCSTM](https://github.com/jmid/qcstm)   
- [Model_quickcheck](https://github.com/suttonshire/model_quickcheck)
- ATTAPL Chapter 8
- The *Real World OCaml* textbook 
- [Cornell CS 3110 OCaml textbook](https://cs3110.github.io/textbook/chapters/ds/hash_tables.html#maps-as-hash-tables)
- [Penn CS 1200 lecture notes](https://www.seas.upenn.edu/~cis120/23su/files/120notes.pdf#page=3)
- [Yale-NUS YSC2229 website](https://ilyasergey.net/YSC2229/week-11-bst.html)
- [Jean-Christophe Filliatre's polynomial implementation](https://www.lri.fr/~filliatr/ftp/ocaml/ds/poly.ml.html)
- [Shayne Fletcher's polynomial implementation](https://blog.shaynefletcher.org/2017/03/polynomials-over-rings.html)

