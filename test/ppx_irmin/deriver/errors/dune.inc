(env
 (_
  (env-vars
   (OCAML_ERROR_STYLE "short")
   (OCAML_COLOR "never"))))

; -------- Test: `lib_invalid.ml` --------



; Run the PPX on the `.ml` file
(rule
 (targets lib_invalid.actual)
 (deps
  (:pp pp.exe)
  (:input lib_invalid.ml))
 (action
  ; expect the process to fail, capturing stderr
  (with-stderr-to
   %{targets}
   (bash "! ./%{pp} -no-color --impl %{input}"))))

; Compare the post-processed output to the .expected file
(alias
 (name runtest)
 (package ppx_irmin)
 (action
  (diff lib_invalid.expected lib_invalid.actual)))

; -------- Test: `nobuiltin_nonempty.ml` --------



; Run the PPX on the `.ml` file
(rule
 (targets nobuiltin_nonempty.actual)
 (deps
  (:pp pp.exe)
  (:input nobuiltin_nonempty.ml))
 (action
  ; expect the process to fail, capturing stderr
  (with-stderr-to
   %{targets}
   (bash "! ./%{pp} -no-color --impl %{input}"))))

; Compare the post-processed output to the .expected file
(alias
 (name runtest)
 (package ppx_irmin)
 (action
  (diff nobuiltin_nonempty.expected nobuiltin_nonempty.actual)))

; -------- Test: `unsupported_tuple_size.ml` --------



; Run the PPX on the `.ml` file
(rule
 (targets unsupported_tuple_size.actual)
 (deps
  (:pp pp.exe)
  (:input unsupported_tuple_size.ml))
 (action
  ; expect the process to fail, capturing stderr
  (with-stderr-to
   %{targets}
   (bash "! ./%{pp} -no-color --impl %{input}"))))

; Compare the post-processed output to the .expected file
(alias
 (name runtest)
 (package ppx_irmin)
 (action
  (diff unsupported_tuple_size.expected unsupported_tuple_size.actual)))

; -------- Test: `unsupported_type_arrow.ml` --------



; Run the PPX on the `.ml` file
(rule
 (targets unsupported_type_arrow.actual)
 (deps
  (:pp pp.exe)
  (:input unsupported_type_arrow.ml))
 (action
  ; expect the process to fail, capturing stderr
  (with-stderr-to
   %{targets}
   (bash "! ./%{pp} -no-color --impl %{input}"))))

; Compare the post-processed output to the .expected file
(alias
 (name runtest)
 (package ppx_irmin)
 (action
  (diff unsupported_type_arrow.expected unsupported_type_arrow.actual)))

; -------- Test: `unsupported_type_extension.ml` --------



; Run the PPX on the `.ml` file
(rule
 (targets unsupported_type_extension.actual)
 (deps
  (:pp pp.exe)
  (:input unsupported_type_extension.ml))
 (action
  ; expect the process to fail, capturing stderr
  (with-stderr-to
   %{targets}
   (bash "! ./%{pp} -no-color --impl %{input}"))))

; Compare the post-processed output to the .expected file
(alias
 (name runtest)
 (package ppx_irmin)
 (action
  (diff unsupported_type_extension.expected unsupported_type_extension.actual)))

; -------- Test: `unsupported_type_open.ml` --------



; Run the PPX on the `.ml` file
(rule
 (targets unsupported_type_open.actual)
 (deps
  (:pp pp.exe)
  (:input unsupported_type_open.ml))
 (action
  ; expect the process to fail, capturing stderr
  (with-stderr-to
   %{targets}
   (bash "! ./%{pp} -no-color --impl %{input}"))))

; Compare the post-processed output to the .expected file
(alias
 (name runtest)
 (package ppx_irmin)
 (action
  (diff unsupported_type_open.expected unsupported_type_open.actual)))

; -------- Test: `unsupported_type_open_polyvariant.ml` --------



; Run the PPX on the `.ml` file
(rule
 (targets unsupported_type_open_polyvariant.actual)
 (deps
  (:pp pp.exe)
  (:input unsupported_type_open_polyvariant.ml))
 (action
  ; expect the process to fail, capturing stderr
  (with-stderr-to
   %{targets}
   (bash "! ./%{pp} -no-color --impl %{input}"))))

; Compare the post-processed output to the .expected file
(alias
 (name runtest)
 (package ppx_irmin)
 (action
  (diff unsupported_type_open_polyvariant.expected
    unsupported_type_open_polyvariant.actual)))

; -------- Test: `unsupported_type_package.ml` --------



; Run the PPX on the `.ml` file
(rule
 (targets unsupported_type_package.actual)
 (deps
  (:pp pp.exe)
  (:input unsupported_type_package.ml))
 (action
  ; expect the process to fail, capturing stderr
  (with-stderr-to
   %{targets}
   (bash "! ./%{pp} -no-color --impl %{input}"))))

; Compare the post-processed output to the .expected file
(alias
 (name runtest)
 (package ppx_irmin)
 (action
  (diff unsupported_type_package.expected unsupported_type_package.actual)))

; -------- Test: `unsupported_type_poly.ml` --------



; Run the PPX on the `.ml` file
(rule
 (targets unsupported_type_poly.actual)
 (deps
  (:pp pp.exe)
  (:input unsupported_type_poly.ml))
 (action
  ; expect the process to fail, capturing stderr
  (with-stderr-to
   %{targets}
   (bash "! ./%{pp} -no-color --impl %{input}"))))

; Compare the post-processed output to the .expected file
(alias
 (name runtest)
 (package ppx_irmin)
 (action
  (diff unsupported_type_poly.expected unsupported_type_poly.actual)))

; -------- Test: `unsupported_type_variable.ml` --------



; Run the PPX on the `.ml` file
(rule
 (targets unsupported_type_variable.actual)
 (deps
  (:pp pp.exe)
  (:input unsupported_type_variable.ml))
 (action
  ; expect the process to fail, capturing stderr
  (with-stderr-to
   %{targets}
   (bash "! ./%{pp} -no-color --impl %{input}"))))

; Compare the post-processed output to the .expected file
(alias
 (name runtest)
 (package ppx_irmin)
 (action
  (diff unsupported_type_variable.expected unsupported_type_variable.actual)))


