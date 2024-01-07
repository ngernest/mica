; -------- Test: `another_passing_test.ml` --------; The executable under test
(executable
 (name another_passing_test)
 (modules another_passing_test)
 (preprocess (pps ppx_template)))

; Run the PPX on the `.ml` file
(rule
 (targets another_passing_test.actual)
 (deps
  (:pp pp.exe)
  (:input another_passing_test.ml))
 (action
  ; expect the process to succeed, captured in target
  (run ./%{pp} --impl %{input} -o %{targets})))

; Compare the post-processed output to the .expected file
(rule
 (alias runtest)
 (package ppx_template)
 (action
  (diff another_passing_test.expected another_passing_test.actual)))
; Ensure that the post-processed executable runs correctly
(rule
 (alias runtest)
 (package ppx_template)
 (action
  (run ./another_passing_test.exe)))

; -------- Test: `passing_test.ml` --------; The executable under test
(executable
 (name passing_test)
 (modules passing_test)
 (preprocess (pps ppx_template)))

; Run the PPX on the `.ml` file
(rule
 (targets passing_test.actual)
 (deps
  (:pp pp.exe)
  (:input passing_test.ml))
 (action
  ; expect the process to succeed, captured in target
  (run ./%{pp} --impl %{input} -o %{targets})))

; Compare the post-processed output to the .expected file
(rule
 (alias runtest)
 (package ppx_template)
 (action
  (diff passing_test.expected passing_test.actual)))
; Ensure that the post-processed executable runs correctly
(rule
 (alias runtest)
 (package ppx_template)
 (action
  (run ./passing_test.exe)))


