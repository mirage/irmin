(env
 (_
  (env-vars
   (OCAML_ERROR_STYLE "short")
   (OCAML_COLOR "never"))))

(library
 (name unsupported_tuple_size)
 (modules unsupported_tuple_size))

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
 (action
  (diff unsupported_tuple_size.expected unsupported_tuple_size.actual)))

(library
 (name unsupported_type_arrow)
 (modules unsupported_type_arrow))

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
 (action
  (diff unsupported_type_arrow.expected unsupported_type_arrow.actual)))

(library
 (name unsupported_type_extension)
 (modules unsupported_type_extension))

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
 (action
  (diff unsupported_type_extension.expected unsupported_type_extension.actual)))

(library
 (name unsupported_type_open)
 (modules unsupported_type_open))

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
 (action
  (diff unsupported_type_open.expected unsupported_type_open.actual)))

(library
 (name unsupported_type_open_polyvariant)
 (modules unsupported_type_open_polyvariant))

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
 (action
  (diff unsupported_type_open_polyvariant.expected
    unsupported_type_open_polyvariant.actual)))

(library
 (name unsupported_type_package)
 (modules unsupported_type_package))

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
 (action
  (diff unsupported_type_package.expected unsupported_type_package.actual)))

(library
 (name unsupported_type_poly)
 (modules unsupported_type_poly))

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
 (action
  (diff unsupported_type_poly.expected unsupported_type_poly.actual)))

(library
 (name unsupported_type_variable)
 (modules unsupported_type_variable))

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
 (action
  (diff unsupported_type_variable.expected unsupported_type_variable.actual)))

