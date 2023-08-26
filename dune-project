(lang dune 3.4)

(name mica)

(generate_opam_files true)

(source
 (github ngernest/mica))

(authors "Ernest Ng <ngernest@seas.upenn.edu>")

(maintainers "Ernest Ng <ngernest@seas.upenn.edu>")

(license LICENSE)

(documentation https://ngernest.github.io/mica/)

(package
 (name mica)
 (synopsis "Automated Differential Testing for OCaml Modules")
 (description "A longer description")
 (depends 
   (ocaml
     (>= 4.13)) 
    dune
    (odoc :with-doc)
    angstrom
    base
    base_quickcheck
    pprint
    core)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
