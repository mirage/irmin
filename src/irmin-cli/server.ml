open Lwt.Syntax
open Irmin_server

let () = Irmin.Backend.Watch.set_listen_dir_hook Irmin_watcher.hook

let setup_log style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ());
  ()

let setup_log =
  Cmdliner.Term.(
    const setup_log $ Fmt_cli.style_renderer () $ Logs_cli.level ())

let main ~readonly ~root ~uri ~tls ~store ~contents ~hash ~config_path
    (module Codec : Conn.Codec.S) fingerprint =
  let store, config =
    Resolver.load_config ?root ?config_path ?store ?hash ?contents ()
  in
  let config = Irmin_server.Cli.Conf.v config uri in
  let (module Store : Irmin.Generic_key.S) =
    Resolver.Store.generic_keyed store
  in
  let module Server = Irmin_server_unix.Make_ext (Codec) (Store) in
  if fingerprint then
    Lwt_io.printl
    @@ Server.Command.Conn.Handshake.V1.fingerprint
         (module Store : Irmin.Generic_key.S)
  else
    let tls_config =
      match tls with
      | Some (c, k) -> Some (`Cert_file c, `Key_file k)
      | _ -> None
    in
    let uri = Irmin.Backend.Conf.(get config) Irmin_server.Cli.Conf.Key.uri in
    let config = if readonly then Server.readonly config else config in
    let* server = Server.v ?tls_config ~uri config in
    let root = match root with Some root -> root | None -> "" in
    Logs.app (fun l -> l "Listening on %a, store: %s" Uri.pp_hum uri root);
    Server.serve server

let main readonly root uri tls (store, hash, contents) codec config_path
    fingerprint () =
  let codec =
    match codec with
    | `Bin -> (module Conn.Codec.Bin : Conn.Codec.S)
    | `Json -> (module Conn.Codec.Json)
  in
  Lwt_main.run
  @@ main ~readonly ~root ~uri ~tls ~store ~contents ~hash ~config_path codec
       fingerprint

open Cmdliner

let root =
  let doc =
    Arg.info ~docs:"" ~docv:"PATH" ~doc:"Irmin store path" [ "r"; "root" ]
  in
  Arg.(value @@ opt (some string) None doc)

let readonly =
  let doc =
    Arg.info
      ~doc:
        "Open in read-only mode. This only has an effect when using irmin-pack"
      [ "readonly" ]
  in
  Arg.(value @@ flag doc)

let tls =
  let doc =
    Arg.info ~docs:"" ~docv:"CERT_FILE,KEY_FILE" ~doc:"TLS config" [ "tls" ]
  in
  Arg.(value @@ opt (some (pair string string)) None doc)

let fingerprint =
  let doc =
    Arg.info ~docs:"" ~doc:"Print handshake fingerprint" [ "fingerprint" ]
  in
  Arg.(value @@ flag doc)

let main_term =
  Term.(
    const main
    $ readonly
    $ root
    $ Irmin_server.Cli.uri
    $ tls
    $ Resolver.Store.term ()
    $ Irmin_server.Cli.codec
    $ Irmin_server.Cli.config_path
    $ fingerprint
    $ setup_log)