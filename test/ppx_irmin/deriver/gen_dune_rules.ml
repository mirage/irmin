(* Global configuration for tests in which the PPX fails (for consistency with
   various compiler versions / platforms). *)
let ppx_fail_global_stanzas () =
  Format.printf
    {|(env
 (_
  (env-vars
   (OCAML_ERROR_STYLE "short")
   (OCAML_COLOR "never"))))

|}

let output_stanzas ~expect_failure filename =
  let base = Filename.remove_extension filename in
  let pp_library ppf base =
    (* If the PPX will fail, we don't need to declare the file as executable *)
    if not expect_failure then
      Format.fprintf ppf
        "; The PPX-dependent executable under test@,\
         @[<v 1>(executable@ (name %s)@ (modules %s)@ (preprocess (pps \
         ppx_irmin))@ (libraries irmin))@]"
        base base
    else ()
  in
  let pp_rule ppf base =
    let pp_action ppf expect_failure =
      Format.fprintf ppf
        (if expect_failure then
         "; expect the process to fail, capturing stderr@,\
          @[<v 1>(with-stderr-to@,\
          %%{targets}@,\
          (bash \"! ./%%{pp} -no-color --impl %%{input}\"))@]"
        else
          "(run ./%%{pp} -deriving-keep-w32 both --impl %%{input} -o \
           %%{targets})")
    in
    Format.fprintf ppf
      "; Run the PPX on the `.ml` file@,\
       @[<v 1>(rule@,\
       (targets %s.actual)@,\
       @[<v 1>(deps@,\
       (:pp pp.exe)@,\
       (:input %s.ml))@]@,\
       @[<v 1>(action@,\
       %a))@]@]"
      base base pp_action expect_failure
  in
  let pp_diff_alias ppf base =
    Format.fprintf ppf
      "; Compare the post-processed output to the .expected file@,\
       @[<v 1>(alias@,\
       (name runtest)@,\
       (package ppx_irmin)@,\
       @[<v 1>(action@,\
       @[<hov 2>(diff@ %s.expected@ %s.actual)@])@])@]" base base
  in
  let pp_run_alias ppf base =
    (* If we expect the derivation to succeed, then we should be able to compile
       the output. *)
    if not expect_failure then
      Format.fprintf ppf
        "@,\
         @,\
         ; Ensure that the post-processed executable runs correctly@,\
         @[<v 1>(alias@,\
         (name runtest)@,\
         (package ppx_irmin)@,\
         @[<v 1>(action@,\
         @[<hov 2>(run@ ./%s.exe)@])@])@]" base
    else ()
  in
  Format.set_margin 80;
  Format.printf
    "@[<v 0>; -------- Test: `%s.ml` --------@,@,%a@,@,%a@,@,%a%a@,@]@." base
    pp_library base pp_rule base pp_diff_alias base pp_run_alias base

let is_error_test = function
  | "pp.ml" -> false
  | "gen_dune_rules.ml" -> false
  | filename ->
      Filename.check_suffix filename ".ml"
      (* Avoid capturing post-PPX files *)
      && not (Filename.check_suffix filename ".pp.ml")

let () =
  let expect_failure =
    match Array.to_list Sys.argv with
    | [ _; "--expect-failure" ] -> true
    | [ _ ] -> false
    | _ -> failwith "Unsupported option passed"
  in
  if expect_failure then ppx_fail_global_stanzas ();
  Sys.readdir "."
  |> Array.to_list
  |> List.sort String.compare
  |> List.filter is_error_test
  |> List.iter (output_stanzas ~expect_failure);
  Format.printf "\n%!"
