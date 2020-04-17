(env
 (_
  (env-vars
   (OCAML_ERROR_STYLE "short")
   (OCAML_COLOR "never"))))

(library
 (name alias)
 (modules alias))

(rule
 (targets alias.actual)
 (deps
  (:pp pp.exe)
  (:input alias.ml))
 (action
  (run ./%{pp} -deriving-keep-w32 both --impl %{input} -o %{targets})))

(alias
 (name runtest)
 (package ppx_irmin)
 (action
  (diff alias.expected alias.actual)))

(library
 (name arguments)
 (modules arguments))

(rule
 (targets arguments.actual)
 (deps
  (:pp pp.exe)
  (:input arguments.ml))
 (action
  (run ./%{pp} -deriving-keep-w32 both --impl %{input} -o %{targets})))

(alias
 (name runtest)
 (package ppx_irmin)
 (action
  (diff arguments.expected arguments.actual)))

(library
 (name basic)
 (modules basic))

(rule
 (targets basic.actual)
 (deps
  (:pp pp.exe)
  (:input basic.ml))
 (action
  (run ./%{pp} -deriving-keep-w32 both --impl %{input} -o %{targets})))

(alias
 (name runtest)
 (package ppx_irmin)
 (action
  (diff basic.expected basic.actual)))

(library
 (name composite)
 (modules composite))

(rule
 (targets composite.actual)
 (deps
  (:pp pp.exe)
  (:input composite.ml))
 (action
  (run ./%{pp} -deriving-keep-w32 both --impl %{input} -o %{targets})))

(alias
 (name runtest)
 (package ppx_irmin)
 (action
  (diff composite.expected composite.actual)))

(library
 (name lib_relocated)
 (modules lib_relocated))

(rule
 (targets lib_relocated.actual)
 (deps
  (:pp pp.exe)
  (:input lib_relocated.ml))
 (action
  (run ./%{pp} -deriving-keep-w32 both --impl %{input} -o %{targets})))

(alias
 (name runtest)
 (package ppx_irmin)
 (action
  (diff lib_relocated.expected lib_relocated.actual)))

(library
 (name module)
 (modules module))

(rule
 (targets module.actual)
 (deps
  (:pp pp.exe)
  (:input module.ml))
 (action
  (run ./%{pp} -deriving-keep-w32 both --impl %{input} -o %{targets})))

(alias
 (name runtest)
 (package ppx_irmin)
 (action
  (diff module.expected module.actual)))

(library
 (name nobuiltin)
 (modules nobuiltin))

(rule
 (targets nobuiltin.actual)
 (deps
  (:pp pp.exe)
  (:input nobuiltin.ml))
 (action
  (run ./%{pp} -deriving-keep-w32 both --impl %{input} -o %{targets})))

(alias
 (name runtest)
 (package ppx_irmin)
 (action
  (diff nobuiltin.expected nobuiltin.actual)))

(library
 (name nonrec)
 (modules nonrec))

(rule
 (targets nonrec.actual)
 (deps
  (:pp pp.exe)
  (:input nonrec.ml))
 (action
  (run ./%{pp} -deriving-keep-w32 both --impl %{input} -o %{targets})))

(alias
 (name runtest)
 (package ppx_irmin)
 (action
  (diff nonrec.expected nonrec.actual)))

(library
 (name polyvariant)
 (modules polyvariant))

(rule
 (targets polyvariant.actual)
 (deps
  (:pp pp.exe)
  (:input polyvariant.ml))
 (action
  (run ./%{pp} -deriving-keep-w32 both --impl %{input} -o %{targets})))

(alias
 (name runtest)
 (package ppx_irmin)
 (action
  (diff polyvariant.expected polyvariant.actual)))

(library
 (name record)
 (modules record))

(rule
 (targets record.actual)
 (deps
  (:pp pp.exe)
  (:input record.ml))
 (action
  (run ./%{pp} -deriving-keep-w32 both --impl %{input} -o %{targets})))

(alias
 (name runtest)
 (package ppx_irmin)
 (action
  (diff record.expected record.actual)))

(library
 (name signature)
 (modules signature))

(rule
 (targets signature.actual)
 (deps
  (:pp pp.exe)
  (:input signature.ml))
 (action
  (run ./%{pp} -deriving-keep-w32 both --impl %{input} -o %{targets})))

(alias
 (name runtest)
 (package ppx_irmin)
 (action
  (diff signature.expected signature.actual)))

(library
 (name tuple_deep)
 (modules tuple_deep))

(rule
 (targets tuple_deep.actual)
 (deps
  (:pp pp.exe)
  (:input tuple_deep.ml))
 (action
  (run ./%{pp} -deriving-keep-w32 both --impl %{input} -o %{targets})))

(alias
 (name runtest)
 (package ppx_irmin)
 (action
  (diff tuple_deep.expected tuple_deep.actual)))

(library
 (name variant)
 (modules variant))

(rule
 (targets variant.actual)
 (deps
  (:pp pp.exe)
  (:input variant.ml))
 (action
  (run ./%{pp} -deriving-keep-w32 both --impl %{input} -o %{targets})))

(alias
 (name runtest)
 (package ppx_irmin)
 (action
  (diff variant.expected variant.actual)))

