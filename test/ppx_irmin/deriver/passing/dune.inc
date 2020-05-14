; -------- Test: `alias.ml` --------

; The PPX-dependent executable under test
(executable
 (name alias)
 (modules alias)
 (preprocess (pps ppx_irmin))
 (libraries irmin))

; Run the PPX on the `.ml` file
(rule
 (targets alias.actual)
 (deps
  (:pp pp.exe)
  (:input alias.ml))
 (action
  (run ./%{pp} -deriving-keep-w32 both --impl %{input} -o %{targets})))

; Compare the post-processed output to the .expected file
(alias
 (name runtest)
 (package ppx_irmin)
 (action
  (diff alias.expected alias.actual)))

; Ensure that the post-processed executable runs correctly
(alias
 (name runtest)
 (package ppx_irmin)
 (action
  (run ./alias.exe)))

; -------- Test: `arguments.ml` --------

; The PPX-dependent executable under test
(executable
 (name arguments)
 (modules arguments)
 (preprocess (pps ppx_irmin))
 (libraries irmin))

; Run the PPX on the `.ml` file
(rule
 (targets arguments.actual)
 (deps
  (:pp pp.exe)
  (:input arguments.ml))
 (action
  (run ./%{pp} -deriving-keep-w32 both --impl %{input} -o %{targets})))

; Compare the post-processed output to the .expected file
(alias
 (name runtest)
 (package ppx_irmin)
 (action
  (diff arguments.expected arguments.actual)))

; Ensure that the post-processed executable runs correctly
(alias
 (name runtest)
 (package ppx_irmin)
 (action
  (run ./arguments.exe)))

; -------- Test: `basic.ml` --------

; The PPX-dependent executable under test
(executable
 (name basic)
 (modules basic)
 (preprocess (pps ppx_irmin))
 (libraries irmin))

; Run the PPX on the `.ml` file
(rule
 (targets basic.actual)
 (deps
  (:pp pp.exe)
  (:input basic.ml))
 (action
  (run ./%{pp} -deriving-keep-w32 both --impl %{input} -o %{targets})))

; Compare the post-processed output to the .expected file
(alias
 (name runtest)
 (package ppx_irmin)
 (action
  (diff basic.expected basic.actual)))

; Ensure that the post-processed executable runs correctly
(alias
 (name runtest)
 (package ppx_irmin)
 (action
  (run ./basic.exe)))

; -------- Test: `composite.ml` --------

; The PPX-dependent executable under test
(executable
 (name composite)
 (modules composite)
 (preprocess (pps ppx_irmin))
 (libraries irmin))

; Run the PPX on the `.ml` file
(rule
 (targets composite.actual)
 (deps
  (:pp pp.exe)
  (:input composite.ml))
 (action
  (run ./%{pp} -deriving-keep-w32 both --impl %{input} -o %{targets})))

; Compare the post-processed output to the .expected file
(alias
 (name runtest)
 (package ppx_irmin)
 (action
  (diff composite.expected composite.actual)))

; Ensure that the post-processed executable runs correctly
(alias
 (name runtest)
 (package ppx_irmin)
 (action
  (run ./composite.exe)))

; -------- Test: `lib_relocated.ml` --------

; The PPX-dependent executable under test
(executable
 (name lib_relocated)
 (modules lib_relocated)
 (preprocess (pps ppx_irmin))
 (libraries irmin))

; Run the PPX on the `.ml` file
(rule
 (targets lib_relocated.actual)
 (deps
  (:pp pp.exe)
  (:input lib_relocated.ml))
 (action
  (run ./%{pp} -deriving-keep-w32 both --impl %{input} -o %{targets})))

; Compare the post-processed output to the .expected file
(alias
 (name runtest)
 (package ppx_irmin)
 (action
  (diff lib_relocated.expected lib_relocated.actual)))

; Ensure that the post-processed executable runs correctly
(alias
 (name runtest)
 (package ppx_irmin)
 (action
  (run ./lib_relocated.exe)))

; -------- Test: `module.ml` --------

; The PPX-dependent executable under test
(executable
 (name module)
 (modules module)
 (preprocess (pps ppx_irmin))
 (libraries irmin))

; Run the PPX on the `.ml` file
(rule
 (targets module.actual)
 (deps
  (:pp pp.exe)
  (:input module.ml))
 (action
  (run ./%{pp} -deriving-keep-w32 both --impl %{input} -o %{targets})))

; Compare the post-processed output to the .expected file
(alias
 (name runtest)
 (package ppx_irmin)
 (action
  (diff module.expected module.actual)))

; Ensure that the post-processed executable runs correctly
(alias
 (name runtest)
 (package ppx_irmin)
 (action
  (run ./module.exe)))

; -------- Test: `nobuiltin.ml` --------

; The PPX-dependent executable under test
(executable
 (name nobuiltin)
 (modules nobuiltin)
 (preprocess (pps ppx_irmin))
 (libraries irmin))

; Run the PPX on the `.ml` file
(rule
 (targets nobuiltin.actual)
 (deps
  (:pp pp.exe)
  (:input nobuiltin.ml))
 (action
  (run ./%{pp} -deriving-keep-w32 both --impl %{input} -o %{targets})))

; Compare the post-processed output to the .expected file
(alias
 (name runtest)
 (package ppx_irmin)
 (action
  (diff nobuiltin.expected nobuiltin.actual)))

; Ensure that the post-processed executable runs correctly
(alias
 (name runtest)
 (package ppx_irmin)
 (action
  (run ./nobuiltin.exe)))

; -------- Test: `nonrec.ml` --------

; The PPX-dependent executable under test
(executable
 (name nonrec)
 (modules nonrec)
 (preprocess (pps ppx_irmin))
 (libraries irmin))

; Run the PPX on the `.ml` file
(rule
 (targets nonrec.actual)
 (deps
  (:pp pp.exe)
  (:input nonrec.ml))
 (action
  (run ./%{pp} -deriving-keep-w32 both --impl %{input} -o %{targets})))

; Compare the post-processed output to the .expected file
(alias
 (name runtest)
 (package ppx_irmin)
 (action
  (diff nonrec.expected nonrec.actual)))

; Ensure that the post-processed executable runs correctly
(alias
 (name runtest)
 (package ppx_irmin)
 (action
  (run ./nonrec.exe)))

; -------- Test: `polyvariant.ml` --------

; The PPX-dependent executable under test
(executable
 (name polyvariant)
 (modules polyvariant)
 (preprocess (pps ppx_irmin))
 (libraries irmin))

; Run the PPX on the `.ml` file
(rule
 (targets polyvariant.actual)
 (deps
  (:pp pp.exe)
  (:input polyvariant.ml))
 (action
  (run ./%{pp} -deriving-keep-w32 both --impl %{input} -o %{targets})))

; Compare the post-processed output to the .expected file
(alias
 (name runtest)
 (package ppx_irmin)
 (action
  (diff polyvariant.expected polyvariant.actual)))

; Ensure that the post-processed executable runs correctly
(alias
 (name runtest)
 (package ppx_irmin)
 (action
  (run ./polyvariant.exe)))

; -------- Test: `record.ml` --------

; The PPX-dependent executable under test
(executable
 (name record)
 (modules record)
 (preprocess (pps ppx_irmin))
 (libraries irmin))

; Run the PPX on the `.ml` file
(rule
 (targets record.actual)
 (deps
  (:pp pp.exe)
  (:input record.ml))
 (action
  (run ./%{pp} -deriving-keep-w32 both --impl %{input} -o %{targets})))

; Compare the post-processed output to the .expected file
(alias
 (name runtest)
 (package ppx_irmin)
 (action
  (diff record.expected record.actual)))

; Ensure that the post-processed executable runs correctly
(alias
 (name runtest)
 (package ppx_irmin)
 (action
  (run ./record.exe)))

; -------- Test: `signature.ml` --------

; The PPX-dependent executable under test
(executable
 (name signature)
 (modules signature)
 (preprocess (pps ppx_irmin))
 (libraries irmin))

; Run the PPX on the `.ml` file
(rule
 (targets signature.actual)
 (deps
  (:pp pp.exe)
  (:input signature.ml))
 (action
  (run ./%{pp} -deriving-keep-w32 both --impl %{input} -o %{targets})))

; Compare the post-processed output to the .expected file
(alias
 (name runtest)
 (package ppx_irmin)
 (action
  (diff signature.expected signature.actual)))

; Ensure that the post-processed executable runs correctly
(alias
 (name runtest)
 (package ppx_irmin)
 (action
  (run ./signature.exe)))

; -------- Test: `tuple_deep.ml` --------

; The PPX-dependent executable under test
(executable
 (name tuple_deep)
 (modules tuple_deep)
 (preprocess (pps ppx_irmin))
 (libraries irmin))

; Run the PPX on the `.ml` file
(rule
 (targets tuple_deep.actual)
 (deps
  (:pp pp.exe)
  (:input tuple_deep.ml))
 (action
  (run ./%{pp} -deriving-keep-w32 both --impl %{input} -o %{targets})))

; Compare the post-processed output to the .expected file
(alias
 (name runtest)
 (package ppx_irmin)
 (action
  (diff tuple_deep.expected tuple_deep.actual)))

; Ensure that the post-processed executable runs correctly
(alias
 (name runtest)
 (package ppx_irmin)
 (action
  (run ./tuple_deep.exe)))

; -------- Test: `variant.ml` --------

; The PPX-dependent executable under test
(executable
 (name variant)
 (modules variant)
 (preprocess (pps ppx_irmin))
 (libraries irmin))

; Run the PPX on the `.ml` file
(rule
 (targets variant.actual)
 (deps
  (:pp pp.exe)
  (:input variant.ml))
 (action
  (run ./%{pp} -deriving-keep-w32 both --impl %{input} -o %{targets})))

; Compare the post-processed output to the .expected file
(alias
 (name runtest)
 (package ppx_irmin)
 (action
  (diff variant.expected variant.actual)))

; Ensure that the post-processed executable runs correctly
(alias
 (name runtest)
 (package ppx_irmin)
 (action
  (run ./variant.exe)))


