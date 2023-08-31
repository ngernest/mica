open Lib.Benchmark

(** Launches an executable which benchmarks Mica's code generation functionality
    on the five example modules, along with the corresponding auto-generated
    PBT executables *)
let () = run_mica_qc_bench
