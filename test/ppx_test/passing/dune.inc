; -------- Test: `bst.ml` --------; The executable under test
(executable
 (name bst)
 (modules bst)
 (libraries core)
 (preprocess (pps ppx_mica ppx_jane)))

; Run the PPX on the `.ml` file
(rule
 (targets bst.actual)
 (deps
  (:pp pp.exe)
  (:input bst.ml))
 (action
  ; expect the process to succeed, captured in target
  (run ./%{pp} -deriving-keep-w32 both --impl %{input} -o %{targets})))

; Compare the post-processed output to the .expected file
(rule
 (alias runtest)
 (package ppx_mica)
 (action
  (diff bst.expected bst.actual)))
; Ensure that the post-processed executable runs correctly
(rule
 (alias runtest)
 (package ppx_mica)
 (action
  (run ./bst.exe)))

; -------- Test: `func_with_two_args.ml` --------; The executable under test
(executable
 (name func_with_two_args)
 (modules func_with_two_args)
 (libraries core)
 (preprocess (pps ppx_mica ppx_jane)))

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
 (libraries core)
 (preprocess (pps ppx_mica ppx_jane)))

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

; -------- Test: `polynomial.ml` --------; The executable under test
(executable
 (name polynomial)
 (modules polynomial)
 (libraries core)
 (preprocess (pps ppx_mica ppx_jane)))

; Run the PPX on the `.ml` file
(rule
 (targets polynomial.actual)
 (deps
  (:pp pp.exe)
  (:input polynomial.ml))
 (action
  ; expect the process to succeed, captured in target
  (run ./%{pp} -deriving-keep-w32 both --impl %{input} -o %{targets})))

; Compare the post-processed output to the .expected file
(rule
 (alias runtest)
 (package ppx_mica)
 (action
  (diff polynomial.expected polynomial.actual)))
; Ensure that the post-processed executable runs correctly
(rule
 (alias runtest)
 (package ppx_mica)
 (action
  (run ./polynomial.exe)))

; -------- Test: `regex.ml` --------; The executable under test
(executable
 (name regex)
 (modules regex)
 (libraries core)
 (preprocess (pps ppx_mica ppx_jane)))

; Run the PPX on the `.ml` file
(rule
 (targets regex.actual)
 (deps
  (:pp pp.exe)
  (:input regex.ml))
 (action
  ; expect the process to succeed, captured in target
  (run ./%{pp} -deriving-keep-w32 both --impl %{input} -o %{targets})))

; Compare the post-processed output to the .expected file
(rule
 (alias runtest)
 (package ppx_mica)
 (action
  (diff regex.expected regex.actual)))
; Ensure that the post-processed executable runs correctly
(rule
 (alias runtest)
 (package ppx_mica)
 (action
  (run ./regex.exe)))

; -------- Test: `sets.ml` --------; The executable under test
(executable
 (name sets)
 (modules sets)
 (libraries core)
 (preprocess (pps ppx_mica ppx_jane)))

; Run the PPX on the `.ml` file
(rule
 (targets sets.actual)
 (deps
  (:pp pp.exe)
  (:input sets.ml))
 (action
  ; expect the process to succeed, captured in target
  (run ./%{pp} -deriving-keep-w32 both --impl %{input} -o %{targets})))

; Compare the post-processed output to the .expected file
(rule
 (alias runtest)
 (package ppx_mica)
 (action
  (diff sets.expected sets.actual)))
; Ensure that the post-processed executable runs correctly
(rule
 (alias runtest)
 (package ppx_mica)
 (action
  (run ./sets.exe)))

; -------- Test: `stack.ml` --------; The executable under test
(executable
 (name stack)
 (modules stack)
 (libraries core)
 (preprocess (pps ppx_mica ppx_jane)))

; Run the PPX on the `.ml` file
(rule
 (targets stack.actual)
 (deps
  (:pp pp.exe)
  (:input stack.ml))
 (action
  ; expect the process to succeed, captured in target
  (run ./%{pp} -deriving-keep-w32 both --impl %{input} -o %{targets})))

; Compare the post-processed output to the .expected file
(rule
 (alias runtest)
 (package ppx_mica)
 (action
  (diff stack.expected stack.actual)))
; Ensure that the post-processed executable runs correctly
(rule
 (alias runtest)
 (package ppx_mica)
 (action
  (run ./stack.exe)))

; -------- Test: `two_function_signature.ml` --------; The executable under test
(executable
 (name two_function_signature)
 (modules two_function_signature)
 (libraries core)
 (preprocess (pps ppx_mica ppx_jane)))

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


