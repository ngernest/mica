(lang dune 3.7)
(name ppx_mica)

(generate_opam_files true)
(source (github ngernest/ppx_mica))

(authors "Ernest Ng <eyn5@cornell.edu>")
(maintainers "Ernest Ng <eyn5@cornell.edu>")
(license ISC)

(package
 (name ppx_mica)
 (synopsis "PPX deriver that automates differential testing for OCaml modules")
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
    (>= v5.2.1))
   (ppxlib 
    (>= v0.32.0))
   (alcotest
    (>= v1.7.0))))

(package 
 (name tyche_utils)
 (synopsis "Utilities for Tyche to visualize results produced from tests run by Mica")
 (depends
   dune 
   (yojson 
     (>= v2.0.0))
   (base
     (>= v0.15.1))
   (core_unix
     (>= v0.15.0))))

(package
 (name mica_case_studies)
 (synopsis "Mica case studies (for OCaml Workshop 2024 talk)")
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
    (>= v5.2.1))
   (ppxlib 
    (>= v0.32.0))
   (stdint 
    (>= v0.7.2))
   (integers 
    (>= v0.7.0))
   (charset 
    (>= v0.1.0))))
