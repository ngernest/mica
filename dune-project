(lang dune 2.9)
(name mica)
(generate_opam_files true)
(source (github ngernest/mica))
(authors "Ernest Ng <ngernest@seas.upenn.edu>")
(maintainers "Ernest Ng <ngernest@seas.upenn.edu>")
(license LICENSE)
(documentation https://ngernest.github.io/mica/)
(package
 (name mica)
 (synopsis "Automated Differential Testing for OCaml Modules")
 (description "Mica parses any generic OCaml module signature, and checks whether two modules implementing the signature are observationally equivalent by generating random sequences of commands using Core.QuickCheck.")
 (depends
  (stdio (>= v0.15.0))
  (re (>= 1.10.4))
  (ppx_sexp_conv (>= v0.15.1))
  (ppx_jane (>= v0.15.0))
  (ppx_assert (>= v0.15.0))
  (core_unix (>= v0.15.2))
  (ocaml (>= 4.13))
  dune
  (odoc :with-doc)
  angstrom
  base
  base_quickcheck
  pprint
  core
  core_bench))
