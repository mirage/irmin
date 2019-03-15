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

module Config = struct

  let remote = None

  let info ?(author="test") fmt =
    let d = ref 0 in
    let date = incr d; Int64.of_int !d in
    Fmt.kstrf (fun msg () -> Irmin.Info.v ~date ~author msg) fmt
end

let (/) = Filename.concat

let socket = Filename.get_temp_dir_name () / "irmin-graphql.sock"
let pid_file = Filename.get_temp_dir_name () / "irmin-graphql-test.pid"
let uri = Uri.of_string "http://irmin/graphql"

module Client = struct
  include Cohttp_lwt_unix.Client
  let ctx () =
    let resolver =
      let h = Hashtbl.create 1 in
      Hashtbl.add h "irmin" (`Unix_domain_socket socket);
      Resolver_lwt_unix.static h
    in
    Some (Cohttp_lwt_unix.Client.custom_ctx ~resolver ())
end

let graphql_store (module S: Irmin_test.S) =
  Irmin_test.store
    (module Irmin_graphql.Client.Make(Client))
    (module S.Metadata)

(* See https://github.com/mirage/ocaml-cohttp/issues/511 *)
let () = Lwt.async_exception_hook := (fun e ->
    Fmt.pr "Async exception caught: %a" Fmt.exn e;
  )

let remove file = try Unix.unlink file with _ -> ()

let signal pid =
  let oc = open_out pid_file in
  Logs.debug (fun l -> l "write PID %d in %s" pid pid_file);
  output_string oc (string_of_int pid);
  flush oc;
  close_out oc;
  Lwt.return_unit

let rec wait_for_the_server_to_start () =
  if Sys.file_exists pid_file then (
    let ic = open_in pid_file in
    let line = input_line ic in
    close_in ic;
    let pid = int_of_string line in
    Logs.debug (fun l -> l "read PID %d from %s" pid pid_file);
    Unix.unlink pid_file;
    Lwt.return pid
  ) else (
    Logs.debug (fun l -> l "waiting for the server to start...");
    Lwt_unix.sleep 0.1 >>= fun () ->
    wait_for_the_server_to_start ()
  )

let servers = [
  `Quick, Test_mem.suite;
]

let root c = Irmin.Private.Conf.(get c root)

let serve servers n =
  Logs.set_level ~all:true (Some Logs.Debug);
  Logs.debug (fun l -> l "pwd: %s" @@ Unix.getcwd ());
  let (_, (server : Irmin_test.t)) = List.nth servers n in
  Logs.debug (fun l -> l "Got server: %s, root=%a"
                 server.name Fmt.(option string) (root server.config));
  let (module Server: Irmin_test.S) = server.store in
  let module G =
    Irmin_graphql.Server.Make(Cohttp_lwt_unix.Server)(Config)(Server)
  in
  let server () =
    server.init () >>= fun () ->
    Server.Repo.v server.config >>= fun repo ->
    signal (Unix.getpid ()) >>= fun () ->
    (* XXX(samoht): the server should take a repo, not a t *)
    Server.master repo >>= fun t ->
    let spec = G.v t in
    Lwt.catch
      (fun () -> Lwt_unix.unlink socket)
      (function Unix.Unix_error _ -> Lwt.return () | e -> Lwt.fail e)
    >>= fun () ->
    let mode = `Unix_domain_socket (`File socket) in
    Conduit_lwt_unix.set_max_active 100;
    Cohttp_lwt_unix.Server.create ~mode spec
  in
  Lwt_main.run (server ())

let suite i server =
  let open Irmin_test in
  let server_pid = ref 0 in
  { name = Printf.sprintf "GRAPHQL.%s" server.name;

    init = begin fun () ->
      remove pid_file;
      Lwt_io.flush_all () >>= fun () ->
      let _ = Sys.command @@ Fmt.strf "dune exec --root . -- %s serve %d &" Sys.argv.(0) i in
      wait_for_the_server_to_start () >|= fun pid ->
      server_pid := pid
    end;

    stats = None;
    clean = begin fun () ->
      try Unix.kill !server_pid Sys.sigkill;
        let () =
          try ignore (Unix.waitpid [Unix.WUNTRACED] !server_pid)
          with _ -> () in
        server.clean ()
      with Unix.Unix_error (Unix.ESRCH, _, _) -> Lwt.return_unit
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
    List.mapi (fun i (s, server) ->
        s, suite i server
      ) servers

let with_server servers f =
  if Array.length Sys.argv = 3 && Sys.argv.(1) = "serve" then
    let n = int_of_string Sys.argv.(2) in
    Logs.set_reporter (Irmin_test.reporter ~prefix:"S" ());
    serve servers n
  else
    f ()
