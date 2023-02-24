# Property-Based Testing for OCaml Modules (WIP)

This repo contains experimental code related to property-based testing for OCaml modules.

## Description of repo
`TestHarness.ml` contains a minimal port of QCSTM to Jane Street infrastructure is located in. 

The `lib` directory also contains examples of the test harness adapted to work with the following modules (work in progress):
- Stack
- Hash tables
- Queue
- PriorityQueue (implementation by Harry Goldstein, WIP)
- Fqueue (purely functional queues, code from Real World OCaml, WIP)
- Foldable (aka Container in Jane Street's Core library) (code from Real World OCaml, WIP)


## References
- Jane Street's [Base_quickcheck](https://opensource.janestreet.com/base_quickcheck/)
- [QCSTM](https://github.com/jmid/qcstm)   
- [Model_quickcheck](https://github.com/suttonshire/model_quickcheck)
- ATTAPL Chapter 8
- Real World OCaml (Chapters 5, 10, 11, 15, 17)
- [An Introduction to the OCaml PPX Ecosystem (Tarides)](https://tarides.com/blog/2019-05-09-an-introduction-to-ocaml-ppx-ecosystem)
- [Ppxlib user manual](https://ocaml-ppx.github.io/ppxlib/ppxlib/index.html)



