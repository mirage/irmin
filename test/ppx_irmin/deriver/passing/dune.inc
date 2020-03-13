(env
 (_
  (env-vars
   (OCAML_ERROR_STYLE "short")
   (OCAML_COLOR "never"))))

(executable
 (name alias)
 (modules alias)
 (preprocess (pps ppx_irmin))
 (libraries irmin))

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

(alias
 (name runtest)
 (package ppx_irmin)
 (action
  (run ./alias.exe)))

(executable
 (name arguments)
 (modules arguments)
 (preprocess (pps ppx_irmin))
 (libraries irmin))

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

(alias
 (name runtest)
 (package ppx_irmin)
 (action
  (run ./arguments.exe)))

(executable
 (name basic)
 (modules basic)
 (preprocess (pps ppx_irmin))
 (libraries irmin))

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

(alias
 (name runtest)
 (package ppx_irmin)
 (action
  (run ./basic.exe)))

(executable
 (name composite)
 (modules composite)
 (preprocess (pps ppx_irmin))
 (libraries irmin))

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

(alias
 (name runtest)
 (package ppx_irmin)
 (action
  (run ./composite.exe)))

(executable
 (name lib_relocated)
 (modules lib_relocated)
 (preprocess (pps ppx_irmin))
 (libraries irmin))

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

(alias
 (name runtest)
 (package ppx_irmin)
 (action
  (run ./lib_relocated.exe)))

(executable
 (name module)
 (modules module)
 (preprocess (pps ppx_irmin))
 (libraries irmin))

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

(alias
 (name runtest)
 (package ppx_irmin)
 (action
  (run ./module.exe)))

(executable
 (name nobuiltin)
 (modules nobuiltin)
 (preprocess (pps ppx_irmin))
 (libraries irmin))

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

(alias
 (name runtest)
 (package ppx_irmin)
 (action
  (run ./nobuiltin.exe)))

(executable
 (name nonrec)
 (modules nonrec)
 (preprocess (pps ppx_irmin))
 (libraries irmin))

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

(alias
 (name runtest)
 (package ppx_irmin)
 (action
  (run ./nonrec.exe)))

(executable
 (name polyvariant)
 (modules polyvariant)
 (preprocess (pps ppx_irmin))
 (libraries irmin))

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

(alias
 (name runtest)
 (package ppx_irmin)
 (action
  (run ./polyvariant.exe)))

(executable
 (name record)
 (modules record)
 (preprocess (pps ppx_irmin))
 (libraries irmin))

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

(alias
 (name runtest)
 (package ppx_irmin)
 (action
  (run ./record.exe)))

(executable
 (name signature)
 (modules signature)
 (preprocess (pps ppx_irmin))
 (libraries irmin))

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

(alias
 (name runtest)
 (package ppx_irmin)
 (action
  (run ./signature.exe)))

(executable
 (name tuple_deep)
 (modules tuple_deep)
 (preprocess (pps ppx_irmin))
 (libraries irmin))

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

(alias
 (name runtest)
 (package ppx_irmin)
 (action
  (run ./tuple_deep.exe)))

(executable
 (name variant)
 (modules variant)
 (preprocess (pps ppx_irmin))
 (libraries irmin))

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

(alias
 (name runtest)
 (package ppx_irmin)
 (action
  (run ./variant.exe)))

