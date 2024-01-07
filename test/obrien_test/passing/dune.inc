; -------- Test: `func_with_two_args.ml` --------; The executable under test
(executable
 (name func_with_two_args)
 (modules func_with_two_args)
 (preprocess (pps ppx_mica)))

; Run the PPX on the `.ml` file
(rule
 (targets func_with_two_args.actual)
 (deps
  (:pp pp.exe)
  (:input func_with_two_args.ml))
 (action
  ; expect the process to succeed, captured in target
  (run ./%{pp} -deriving-keep-w32 both --impl %{input} -o %{targets})))

; Compare the post-processed output to the .expected file
(rule
 (alias runtest)
 (package ppx_mica)
 (action
  (diff func_with_two_args.expected func_with_two_args.actual)))
; Ensure that the post-processed executable runs correctly
(rule
 (alias runtest)
 (package ppx_mica)
 (action
  (run ./func_with_two_args.exe)))

; -------- Test: `one_function_signature.ml` --------; The executable under test
(executable
 (name one_function_signature)
 (modules one_function_signature)
 (preprocess (pps ppx_mica)))

; Run the PPX on the `.ml` file
(rule
 (targets one_function_signature.actual)
 (deps
  (:pp pp.exe)
  (:input one_function_signature.ml))
 (action
  ; expect the process to succeed, captured in target
  (run ./%{pp} -deriving-keep-w32 both --impl %{input} -o %{targets})))

; Compare the post-processed output to the .expected file
(rule
 (alias runtest)
 (package ppx_mica)
 (action
  (diff one_function_signature.expected one_function_signature.actual)))
; Ensure that the post-processed executable runs correctly
(rule
 (alias runtest)
 (package ppx_mica)
 (action
  (run ./one_function_signature.exe)))

; -------- Test: `two_function_signature.ml` --------; The executable under test
(executable
 (name two_function_signature)
 (modules two_function_signature)
 (preprocess (pps ppx_mica)))

; Run the PPX on the `.ml` file
(rule
 (targets two_function_signature.actual)
 (deps
  (:pp pp.exe)
  (:input two_function_signature.ml))
 (action
  ; expect the process to succeed, captured in target
  (run ./%{pp} -deriving-keep-w32 both --impl %{input} -o %{targets})))

; Compare the post-processed output to the .expected file
(rule
 (alias runtest)
 (package ppx_mica)
 (action
  (diff two_function_signature.expected two_function_signature.actual)))
; Ensure that the post-processed executable runs correctly
(rule
 (alias runtest)
 (package ppx_mica)
 (action
  (run ./two_function_signature.exe)))


