; -------- Test: `empty_signature.ml` --------

; Run the PPX on the `.ml` file
(rule
 (targets empty_signature.actual)
 (deps
  (:pp pp.exe)
  (:input empty_signature.ml))
 (action
  ; expect the process to fail, capturing stderr
  (with-stderr-to
   %{targets}
   (bash "! ./%{pp} -no-color --impl %{input}"))))

; Compare the post-processed output to the .expected file
(rule
 (alias runtest)
 (package ppx_mica)
 (action
  (diff empty_signature.expected empty_signature.actual)))


