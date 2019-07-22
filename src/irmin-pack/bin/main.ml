open Irmin_pack
module IO = Irmin_pack_io.Unix

type t = { context : string }

let check_dict t =
  let t = Dict.v t.context in
  let io = Dict.io t in
  let offset = IO.offset io in
  let version = IO.version io in
  Fmt.pr "%a\noffsets: %Ld\nversion: %s\n"
    Fmt.(styled `Bold string)
    "store.dict" offset version

let check t = check_dict t

open Cmdliner

let reporter ?(prefix = "") () =
  let report src level ~over k msgf =
    let k _ =
      over ();
      k ()
    in
    let ppf = match level with Logs.App -> Fmt.stdout | _ -> Fmt.stderr in
    let with_stamp h _tags k fmt =
      let dt = Unix.gettimeofday () in
      Fmt.kpf k ppf
        ("%s%+04.0fus %a %a @[" ^^ fmt ^^ "@]@.")
        prefix dt
        Fmt.(styled `Magenta string)
        (Logs.Src.name src) Logs_fmt.pp_header (level, h)
    in
    msgf @@ fun ?header ?tags fmt ->
    with_stamp header tags k fmt
  in
  { Logs.report }

let log style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (reporter ());
  ()

let log = Term.(const log $ Fmt_cli.style_renderer () $ Logs_cli.level ())

let context =
  let doc = Arg.info ~doc:"Location of the context." [ "context" ] in
  Arg.(value @@ opt string "context" doc)

let config = Term.(const (fun () context -> { context }) $ log $ context)

let term =
  (Term.(const check $ config), Term.info ~version:"%%VERSION%%" "packachu")

let () = Term.(exit @@ eval term)
