# Example property-based testing code for OCaml modules (work in progress)

This directory contains examples of a property-based testing harness for OCaml modules 
(see `TestHarness.ml` and the associated .mli file). Ideally, we would like 
an automated tool that does the following:

1. Take an OCaml module signature and generate property-based testing code
(in the vein of [QCSTM](https://github.com/jmid/qcstm)) that uses
Jane Street's Core.QuickCheck infrastructure 

2. Take two OCaml modules that implement the same signature, and generate
PBT code that checks whether the two modules are behaviorally / observationally
equal

The `Spec1` and `Spec2` module signatures in `TestHarness.mli` specify 
a module signature that any PBT harness must satisfy. We also have a 
minimal port of QCSTM that uses Jane Street's build infrastructure. 

This directory also contains examples of the PBT harness adapted to work with the following data structures (work in progress):
- Finite sets (three implementations of the same `SetIntf` signature) -- see `Sets.ml`
    - The functor `CompareSetImpls` takes in two modules that both implement the 
      `SetIntf` signature & checks whether they are behaviorally equal
- Stack (three implementations of the same `StackIntf` module signature) -- see `Stack.ml` & `StackTest.ml`
    - `StackTest.ml` contains a module that takes in a module implementing the 
      `StackIntf` signature & uses PBT to check for correctness 


Addendum:
- The `Articheck.ml` file contains code associated with the paper 
  "ArtiCheck: well-typed generic fuzzing for module interfaces"
  (Braibant et al, 2014), which we are currently reading. 



