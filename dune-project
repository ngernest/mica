(lang dune 3.7)
(name ppx_mica)

(generate_opam_files true)
(source (github ngernest/ppx_mica))

(authors "Ernest Ng <eyn5@cornell.edu>")
(maintainers "Ernest Ng <eyn5@cornell.edu>")
(license ISC)

(package
 (name ppx_mica)
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
   (yojson 
    (>= v2.0.0))
   (alcotest
    (>= v1.7.0))))
