let global_stanzas () =
  Fmt.pr
    {|(env
 (_
  (env-vars
   (OCAML_ERROR_STYLE "short")
   (OCAML_COLOR "never"))))
|}

let output_stanzas ~expect_failure filename =
  let base = Filename.remove_extension filename in
  let pp_library ppf base =
    if not expect_failure then
      Fmt.pf ppf "@[<v 1>(library@ (name %s)@ (modules %s))@]" base base
    else ()
  in
  let pp_rule ppf base =
    let pp_action ppf expect_failure =
      Fmt.pf ppf
        ( if expect_failure then
          "@[<v 1>(with-stderr-to@,\
           %%{targets}@,\
           (bash \"! ./%%{pp} -no-color --impl %%{input}\"))@]"
        else
          "(run ./%%{pp} -deriving-keep-w32 both --impl %%{input} -o \
           %%{targets})" )
    in
    Fmt.pf ppf
      "@[<v 1>(rule@,\
       (targets %s.actual)@,\
       @[<v 1>(deps@,\
       (:pp pp.exe)@,\
       (:input %s.ml))@]@,\
       @[<v 1>(action@,\
       %a))@]@]"
      base base pp_action expect_failure
  in
  let pp_alias ppf base =
    Fmt.pf ppf
      "@[<v 1>(alias@,\
       (name runtest)@,\
       @[<v 1>(action@,\
       @[<hov 2>(diff@ %s.expected@ %s.actual)@])@])@]"
      base base
  in
  Format.set_margin 80;
  Fmt.pr "@[<v 0>@,%a@,@,%a@,@,%a@]@." pp_library base pp_rule base pp_alias
    base

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
  global_stanzas ();
  Sys.readdir "."
  |> Array.to_list
  |> List.sort String.compare
  |> List.filter is_error_test
  |> List.iter (output_stanzas ~expect_failure);
  Fmt.pr "\n%!"
