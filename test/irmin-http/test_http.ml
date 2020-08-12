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

let ( / ) = Filename.concat

let test_http_dir = "test-http"

let uri = Uri.of_string "http://irmin"

let socket name = test_http_dir / Fmt.strf "irmin-%s.sock" name

let pid_file name = test_http_dir / Fmt.strf "irmin-test-%s.pid" name

let pid_file_tmp name = pid_file name ^ ".tmp"

module Client (P : sig
  val name : string
end) =
struct
  include Cohttp_lwt_unix.Client

  let ctx () =
    let resolver =
      let h = Hashtbl.create 1 in
      Hashtbl.add h "irmin" (`Unix_domain_socket (socket P.name));
      Resolver_lwt_unix.static h
    in
    Some (Cohttp_lwt_unix.Client.custom_ctx ~resolver ())
end

let http_store name (module S : Irmin_test.S) =
  let module P = struct
    let name = name
  end in
  let module M = Irmin_http.Client (Client (P)) (S) in
  (module M : Irmin_test.S)

let remove file = try Unix.unlink file with _ -> ()

let rec wait_for_the_server_to_start name =
  let pid_file = pid_file name in
  if Sys.file_exists pid_file then (
    let ic = open_in pid_file in
    let line = input_line ic in
    close_in ic;
    let pid = int_of_string line in
    Logs.debug (fun l -> l "read PID %d from %s" pid pid_file);
    Unix.unlink pid_file;
    Lwt.return pid)
  else (
    Logs.debug (fun l -> l "waiting for the server to start...");
    Lwt_unix.sleep 0.1 >>= fun () -> wait_for_the_server_to_start name)

let servers = [ (`Quick, Test_mem.suite); (`Quick, Test_git.suite) ]

let root c = Irmin.Private.Conf.(get c root)

let mkdir d =
  Lwt.catch
    (fun () -> Lwt_unix.mkdir d 0o755)
    (function
      | Unix.Unix_error (Unix.EEXIST, _, _) -> Lwt.return_unit | e -> Lwt.fail e)

let rec lock name =
  let pid = string_of_int (Unix.getpid ()) in
  let pid_len = String.length pid in
  let pid_file = pid_file name in
  let pid_file_tmp = pid_file_tmp name in
  (* [fd0]'s [O_CREAT] ensures that we are the only one writing to that file *)
  Lwt_unix.openfile pid_file [ Unix.O_CREAT; Unix.O_RDWR ] 0o600 >>= fun fd0 ->
  (* [fd] is used to write the actual PID file; the file is renamed
     bellow to ensure atomicity. *)
  Lwt_unix.openfile pid_file_tmp [ Unix.O_CREAT; Unix.O_RDWR ] 0o600
  >>= fun fd ->
  Lwt.catch
    (fun () ->
      Lwt_unix.lockf fd Unix.F_LOCK 0 >>= fun () ->
      Logs.debug (fun l -> l "write PID %s in %s" pid pid_file);
      Lwt_unix.write fd (Bytes.of_string pid) 0 pid_len >>= fun len ->
      if len <> pid_len then
        Lwt_unix.close fd >>= fun () ->
        Lwt.fail_with "Unable to write PID to lock file"
      else
        Lwt_unix.close fd0 >>= fun () ->
        Lwt_unix.rename pid_file_tmp pid_file >|= fun () -> fd)
    (function
      | Unix.Unix_error (Unix.EAGAIN, _, _) ->
          Lwt_unix.close fd >>= fun () -> lock name
      | e -> Lwt_unix.close fd >>= fun () -> Lwt.fail e)

let unlock fd = Lwt_unix.close fd

let serve servers n =
  Logs.set_level ~all:true (Some Logs.Debug);
  Logs.debug (fun l -> l "pwd: %s" @@ Unix.getcwd ());
  let _, (server : Irmin_test.t) = List.nth servers n in
  Logs.debug (fun l ->
      l "Got server: %s, root=%a" server.name
        Fmt.(option string)
        (root server.config));
  let (module Server : Irmin_test.S) = server.store in
  let module HTTP = Irmin_http.Server (Cohttp_lwt_unix.Server) (Server) in
  let socket = socket server.name in
  let server () =
    server.init () >>= fun () ->
    Server.Repo.v server.config >>= fun repo ->
    lock server.name >>= fun lock ->
    let spec = HTTP.v repo ~strict:false in
    Lwt.catch
      (fun () -> Lwt_unix.unlink socket)
      (function Unix.Unix_error _ -> Lwt.return_unit | e -> Lwt.fail e)
    >>= fun () ->
    let mode = `Unix_domain_socket (`File socket) in
    Conduit_lwt_unix.set_max_active 100;
    Cohttp_lwt_unix.Server.create
      ~on_exn:(Fmt.pr "Async exception caught: %a" Fmt.exn)
      ~mode spec
    >>= fun () -> unlock lock
  in
  Lwt_main.run (server ())

let kill_server pid =
  let () =
    try
      Unix.kill pid Sys.sigkill;
      try ignore (Unix.waitpid [ Unix.WUNTRACED ] pid) with _ -> ()
    with Unix.Unix_error (Unix.ESRCH, _, _) -> ()
  in
  Fmt.epr "Server [PID %d] is killed.\n%!" pid

let suite i server =
  let open Irmin_test in
  let socket = server.name in
  let server_pid = ref 0 in
  {
    name = Printf.sprintf "HTTP.%s" server.name;
    init =
      (fun () ->
        remove socket;
        remove (pid_file server.name);
        mkdir test_http_dir >>= fun () ->
        Lwt_io.flush_all () >>= fun () ->
        let pwd = Sys.getcwd () in
        let root =
          if Filename.basename pwd = "default" then ".." / ".." / "" else ""
        in
        let cmd =
          root ^ ("_build" / "default" / Fmt.strf "%s serve %d &" Sys.argv.(0) i)
        in
        Fmt.epr "pwd=%s\nExecuting: %S\n%!" pwd cmd;
        let _ = Sys.command cmd in
        wait_for_the_server_to_start server.name >|= fun pid ->
        server_pid := pid);
    stats = None;
    clean =
      (fun () ->
        kill_server !server_pid;
        server.clean ());
    config = Irmin_http.config uri;
    store = http_store server.name server.store;
  }

let suites servers =
  if Sys.os_type = "Win32" then
    (* it's a bit hard to test client/server stuff on windows because
       we can't fork. Can work around that later if needed. *)
    []
  else List.mapi (fun i (s, server) -> (s, suite i server)) servers

let with_server servers f =
  if Array.length Sys.argv = 3 && Sys.argv.(1) = "serve" then (
    let n = int_of_string Sys.argv.(2) in
    Logs.set_reporter (Irmin_test.reporter ~prefix:"S" ());
    serve servers n)
  else f ()

type test = Alcotest.speed_level * Irmin_test.t
