(env
 (_
  (env-vars
   (OCAML_ERROR_STYLE "short")
   (OCAML_COLOR "never"))))



(rule
 (targets lib_invalid.actual)
 (deps
  (:pp pp.exe)
  (:input lib_invalid.ml))
 (action
  (with-stderr-to
   %{targets}
   (bash "! ./%{pp} -no-color --impl %{input}"))))

(alias
 (name runtest)
 (package ppx_irmin)
 (action
  (diff lib_invalid.expected lib_invalid.actual)))



(rule
 (targets nobuiltin_nonempty.actual)
 (deps
  (:pp pp.exe)
  (:input nobuiltin_nonempty.ml))
 (action
  (with-stderr-to
   %{targets}
   (bash "! ./%{pp} -no-color --impl %{input}"))))

(alias
 (name runtest)
 (package ppx_irmin)
 (action
  (diff nobuiltin_nonempty.expected nobuiltin_nonempty.actual)))



(rule
 (targets unsupported_tuple_size.actual)
 (deps
  (:pp pp.exe)
  (:input unsupported_tuple_size.ml))
 (action
  (with-stderr-to
   %{targets}
   (bash "! ./%{pp} -no-color --impl %{input}"))))

(alias
 (name runtest)
 (package ppx_irmin)
 (action
  (diff unsupported_tuple_size.expected unsupported_tuple_size.actual)))



(rule
 (targets unsupported_type_arrow.actual)
 (deps
  (:pp pp.exe)
  (:input unsupported_type_arrow.ml))
 (action
  (with-stderr-to
   %{targets}
   (bash "! ./%{pp} -no-color --impl %{input}"))))

(alias
 (name runtest)
 (package ppx_irmin)
 (action
  (diff unsupported_type_arrow.expected unsupported_type_arrow.actual)))



(rule
 (targets unsupported_type_extension.actual)
 (deps
  (:pp pp.exe)
  (:input unsupported_type_extension.ml))
 (action
  (with-stderr-to
   %{targets}
   (bash "! ./%{pp} -no-color --impl %{input}"))))

(alias
 (name runtest)
 (package ppx_irmin)
 (action
  (diff unsupported_type_extension.expected unsupported_type_extension.actual)))



(rule
 (targets unsupported_type_open.actual)
 (deps
  (:pp pp.exe)
  (:input unsupported_type_open.ml))
 (action
  (with-stderr-to
   %{targets}
   (bash "! ./%{pp} -no-color --impl %{input}"))))

(alias
 (name runtest)
 (package ppx_irmin)
 (action
  (diff unsupported_type_open.expected unsupported_type_open.actual)))



(rule
 (targets unsupported_type_open_polyvariant.actual)
 (deps
  (:pp pp.exe)
  (:input unsupported_type_open_polyvariant.ml))
 (action
  (with-stderr-to
   %{targets}
   (bash "! ./%{pp} -no-color --impl %{input}"))))

(alias
 (name runtest)
 (package ppx_irmin)
 (action
  (diff unsupported_type_open_polyvariant.expected
    unsupported_type_open_polyvariant.actual)))



(rule
 (targets unsupported_type_package.actual)
 (deps
  (:pp pp.exe)
  (:input unsupported_type_package.ml))
 (action
  (with-stderr-to
   %{targets}
   (bash "! ./%{pp} -no-color --impl %{input}"))))

(alias
 (name runtest)
 (package ppx_irmin)
 (action
  (diff unsupported_type_package.expected unsupported_type_package.actual)))



(rule
 (targets unsupported_type_poly.actual)
 (deps
  (:pp pp.exe)
  (:input unsupported_type_poly.ml))
 (action
  (with-stderr-to
   %{targets}
   (bash "! ./%{pp} -no-color --impl %{input}"))))

(alias
 (name runtest)
 (package ppx_irmin)
 (action
  (diff unsupported_type_poly.expected unsupported_type_poly.actual)))



(rule
 (targets unsupported_type_variable.actual)
 (deps
  (:pp pp.exe)
  (:input unsupported_type_variable.ml))
 (action
  (with-stderr-to
   %{targets}
   (bash "! ./%{pp} -no-color --impl %{input}"))))

(alias
 (name runtest)
 (package ppx_irmin)
 (action
  (diff unsupported_type_variable.expected unsupported_type_variable.actual)))

