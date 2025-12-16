let test () =
  [%logs.app "Simple log entry"];
  [%logs.err "Log entry on Line %d" __LINE__];
  [%logs.warn "Infix @@ operator: %d" @@ (1 + 2)];
  [%logs.info fun f -> f "Log entry in CPS form: Line %d, %f" __LINE__ 3.14];

  (* Test with a non-immediate application of [f] in the body.

     NOTE: The [Logs] API doesn't support polymorphic continuations: both
     applications of [f] in the body must have the same number and type of
     placeholders. *)
  [%logs.debug
    fun f ->
      if true then f "Everything's OK on line %d" __LINE__
      else f "Something's gone terribly wrong on line %d" __LINE__];
  ()

(* Setup a reporter that can print tags added by the PPX, then run the tests. *)
let () =
  let pp_source_pos ppf (_file, lnum, cnum, enum) =
    Fmt.pf ppf "Line %d, characters %d-%d" lnum cnum enum
  in
  let pp_level =
    Fmt.of_to_string (function
      | Logs.App -> "App"
      | Logs.Error -> "Error"
      | Logs.Warning -> "Warning"
      | Logs.Info -> "Info"
      | Logs.Debug -> "Debug")
  in
  let report _src level ~over k msgf =
    let k _ =
      over ();
      k ()
    in
    msgf @@ fun ?header:_ ?(tags = Logs.Tag.empty) fmt ->
    let source_pos =
      Logs.Tag.find Ppx_irmin_internal_lib.Source_code_position.tag tags
    in
    Fmt.(kpf k stdout)
      ("[%a] [%a] @[" ^^ fmt ^^ "@]@.")
      (Fmt.option pp_source_pos) source_pos pp_level level
  in
  Logs.set_reporter { Logs.report };
  Logs.set_level (Some Debug);
  test ()
