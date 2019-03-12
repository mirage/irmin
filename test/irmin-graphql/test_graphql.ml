(*
 * Copyright (c) 2013-2017 Thomas Gazagnaire <thomas@gazagnaire.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

open Lwt.Infix

module Store = Irmin_unix.Git.Mem.KV(Irmin.Contents.String)
module Client =
  Irmin_unix.Graphql.Client.Make
    (Store.Metadata)
    (Store.Contents)
    (Store.Key)
    (Store.Branch)
    (Store.Hash)

let uri = Uri.of_string "http://localhost:80808/graphql"

(* See https://github.com/mirage/ocaml-cohttp/issues/511 *)
let () = Lwt.async_exception_hook := (fun e ->
    Fmt.pr "Async exception caught: %a" Fmt.exn e;
  )

let ( / ) = Filename.concat
let pid_file = Filename.get_temp_dir_name () / "irmin-graphql-test.pid"

let rec wait_for_the_server_to_start () =
  if Sys.file_exists pid_file then (
    let ic = open_in pid_file in
    let line = input_line ic in
    close_in ic;
    let pid = int_of_string line in
    Logs.debug (fun l -> l "read PID %d from %s" pid pid_file);
    Unix.unlink pid_file;
    Unix.sleep 1;
    pid
  ) else (
    Logs.debug (fun l -> l "waiting for the server to start...");
    Unix.sleep 1;
    wait_for_the_server_to_start ()
  )

let unwrap = function
  | Ok x -> Lwt.return x
  | Error (`Msg e) -> Alcotest.fail e

let check_type_eq name t a b =
  Alcotest.(check bool) name true (Irmin.Type.equal t a b)

let check_type_not_eq name t a b =
  Alcotest.(check bool) name false (Irmin.Type.equal t a b)

let server_pid = ref 0

let clean () = Unix.kill !server_pid Sys.sigint

let graphql_store (module S: Irmin_test.S) =
  Irmin_test.store (module Irmin_unix.Graphql.Client.Make) (module S.Metadata)

let suite server =
  let open Irmin_test in
  { name = Printf.sprintf "GRAPHQL.%s" server.name;

    init = begin fun () ->
      Lwt_io.flush_all ()
    end;

    stats = None;
    clean = begin fun () ->
      Lwt.return_unit
    end;

    config = Irmin_graphql.Client.config uri;
    store = graphql_store server.store;
  }

let suites servers =
  if Sys.os_type = "Win32" then
    (* it's a bit hard to test client/server stuff on windows because
       we can't fork. Can work around that later if needed. *)
    []
  else
    List.map (fun (s, server) ->
        s, suite server
      ) servers

let run_server () =
  let module Server = Irmin_unix.Graphql.Server.Make(Store)(struct let remote = Some (fun ?headers r ->
      let uri = Uri.of_string r in
      match Uri.scheme uri with
      | Some "file" ->
        let cfg = Irmin_git.config (Uri.path uri) in
        let store = Lwt_main.run (Store.Repo.v cfg >>= Store.master) in
        Irmin.remote_store (module Store) store
      | _ -> Store.remote ?headers r)
    end) in
  let server =
    Lwt_unix.on_signal Sys.sigint (fun _ -> exit 0) |> ignore;
    Store.Repo.v (Irmin_mem.config ()) >>= Store.master >>= fun t ->
    server_pid := Unix.getpid ();
    Conduit_lwt_unix.set_max_active 100;
    let server = Server.server t in
    Cohttp_lwt_unix.Server.create ~mode:(`TCP (`Port 80808)) server
  in
  Lwt_main.run server

let servers = [
  `Quick, Test_git.suite;
]

let run () =
  if Sys.os_type = "Win32" then
    (* it's a bit hard to test client/server stuff on windows because
       we can't fork. Can work around that later if needed. *)
    exit 0
  else
  if (Array.length Sys.argv > 1 && Sys.argv.(1) = "server") then
    run_server ()
  else
    let _ = Sys.command (Printf.sprintf "dune exec --root . -- %s server & echo $! > %s" Sys.argv.(0) pid_file) in
    let () = server_pid := wait_for_the_server_to_start () in
    let () = at_exit clean in
    Irmin_test.Store.run "irmin-graphql" ~misc:[] (suites servers)
