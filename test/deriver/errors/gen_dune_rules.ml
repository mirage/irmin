let output_stanzas filename =
  let base = Filename.remove_extension filename in
  Printf.printf
    {|
(library
  (name %s)
  (modules %s)
  (preprocess (pps ppx_yojson))
)

(rule
  (targets %s.actual)
  (deps (:pp pp.exe) (:input %s.ml))
  (action
    (with-stderr-to
      %%{targets}
      (bash "! OCAML_COLOR=never ./%%{pp} -no-color --impl %%{input}")
    )
  )
)

(alias
  (name runtest)
  (action (diff %s.expected %s.actual))
)
|}
    base base base base base base

let is_error_test = function
  | "pp.ml" -> false
  | "gen_dune_rules.ml" -> false
  | filename -> Filename.check_suffix filename ".ml"

let () =
  Sys.readdir "." |> Array.to_list |> List.sort String.compare
  |> List.filter is_error_test |> List.iter output_stanzas
