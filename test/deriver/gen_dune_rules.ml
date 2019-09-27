let output_stanzas ~expect_failure filename =
  let base = Filename.remove_extension filename in
  let pp_action ppf expect_failure =
    Fmt.pf ppf
      ( if expect_failure then
        {|    (setenv "OCAML_ERROR_STYLE" "short"
      (setenv "OCAML_COLOR" "never"
        (with-stderr-to
          %%{targets}
          (bash "! ./%%{pp} -no-color --impl %%{input}")
        )
      )
    )|}
      else
        "    (run ./%%{pp} -deriving-keep-w32 both --impl %%{input} -o \
         %%{targets})" )
  in
  Fmt.pr
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
%a
  )
)

(alias
  (name runtest)
  (action (diff %s.expected %s.actual))
)
|}
    base base base base pp_action expect_failure base base

let is_error_test = function
  | "pp.ml" -> false
  | "gen_dune_rules.ml" -> false
  | filename -> Filename.check_suffix filename ".ml"

let () =
  let expect_failure =
    match Array.to_list Sys.argv with
    | _ :: "--expect-failure" :: _ -> true
    | _ -> false
  in
  Sys.readdir "." |> Array.to_list |> List.sort String.compare
  |> List.filter is_error_test
  |> List.iter (output_stanzas ~expect_failure)
