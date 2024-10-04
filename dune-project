(lang dune 3.7)

(name ppx_mica)

(generate_opam_files true)

(source
 (github ngernest/mica))

(authors "Ernest Ng <ernest@cs.cornell.edu>")

(maintainers "Ernest Ng <ernest@cs.cornell.edu>")
(documentation "https://ngernest.github.io/mica")
(license ISC)

(package
 (name ppx_mica)
 (synopsis
  "PPX deriver that automates differential testing for OCaml modules")
 (depends
  dune
  (ocaml
   (>= 4.13))
  (odoc :with-doc)
  (core
   (>= v0.15.1))
  (ppx_jane
   (>= v0.15.0))
  (ppx_assert
   (>= v0.15.0))
  (base
   (>= v0.15.1))
  (base_quickcheck
   (>= v0.15.0))
  (ppx_deriving
   (>= 6.0.2))
  (ppxlib
   (>= 0.33.0))
  (alcotest
   (and
    :with-test
    (>= 1.8.0)))
  (core_unix
   (>= v0.17.0))))