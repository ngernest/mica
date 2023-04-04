# Property-Based Testing for OCaml Modules (WIP)

This repo contains experimental code related to property-based testing for OCaml modules.

## Description of repo
`TestHarness.ml` contains a minimal port of QCSTM to Jane Street infrastructure.

The `lib` directory also contains examples of the test harness adapted to work with the following modules (work in progress):
- Stack (three implementations of the same signature)
- Sets (three implementations of the same signature)

The `lib/deprecated` sub-directory cotnains some work in progress examples:
- Hash tables
- Queue
- PriorityQueue (implementation by Harry Goldstein, WIP)
- Fqueue (purely functional queues, code from Real World OCaml, WIP)
- Foldable (aka Container in Jane Street's Core library) (code from Real World OCaml, WIP)

The `ppx` directory contains examples of experimental code related to creating
PPX preprocessors using the `ppxlib` library (in particular the `Metaquot` &
`Ast_builder` modules).          
Currently, there are two examples in the `ppx` directory:
- `ppx_accessors`: Deriver for generating accessors/selectors from a record type definition
- `ppx_stringify`: Deriver for generating string representations of lists

## References
- Jane Street's [Base_quickcheck](https://opensource.janestreet.com/base_quickcheck/)
- [QCSTM](https://github.com/jmid/qcstm)   
- [Model_quickcheck](https://github.com/suttonshire/model_quickcheck)
- ATTAPL Chapter 8
- Real World OCaml (Chapters 5, 10, 11, 15, 17)
- [Cornell CS 3110 OCaml textbook](https://cs3110.github.io/textbook/chapters/ds/hash_tables.html#maps-as-hash-tables)
- [An Introduction to the OCaml PPX Ecosystem (Tarides)](https://tarides.com/blog/2019-05-09-an-introduction-to-ocaml-ppx-ecosystem)
- [Ppxlib user manual](https://ocaml-ppx.github.io/ppxlib/ppxlib/index.html)



