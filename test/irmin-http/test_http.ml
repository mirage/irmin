(*
 * Copyright (c) 2013-2021 Thomas Gazagnaire <thomas@gazagnaire.org>
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

open Irmin.Export_for_backends

let () = Random.self_init ()
let ( / ) = Filename.concat
let test_http_dir = "test-http"
let uri = Uri.of_string "http://irmin"

type id = { name : string; id : int }

let pp ppf t = Fmt.pf ppf "%s-%d" t.name t.id
let socket t = test_http_dir / Fmt.strf "irmin-%a.sock" pp t
let pid_file t = test_http_dir / Fmt.strf "irmin-test-%a.pid" pp t
let tmp_file file = file ^ ".tmp"

module Client (P : sig
  val id : id
end) =
struct
  include Cohttp_lwt_unix.Client

  let ctx () =
    let resolver =
      let h = Hashtbl.create 1 in
      Hashtbl.add h "irmin" (`Unix_domain_socket (socket P.id));
      Resolver_lwt_unix.static h
    in
    Some (Cohttp_lwt_unix.Client.custom_ctx ~resolver ())
end

let http_store id (module S : Irmin_test.S) =
  let module P = struct
    let id = id
  end in
  let module M = Irmin_http.Client (Client (P)) (S) in
  (module M : Irmin_test.S)

let remove file = try Unix.unlink file with _ -> ()

let check_connection id =
  let module Client = Client (struct
    let id = id
  end) in
  let ctx = Client.ctx () in
  let rec loop n =
    let url =
      Uri.of_string ("http://irmin/branch/CHECK-CONNECTION-" ^ string_of_int n)
    in
    if n > 10 then
      Alcotest.failf "Cannot connect to server %a: too many retries" pp id
    else
      Lwt.try_bind
        (fun () -> Client.get ?ctx url)
        (fun _ -> Lwt.return ())
        (function
          | Unix.Unix_error (Unix.ECONNREFUSED, _, _) ->
              Lwt_unix.sleep (float n *. 0.1) >>= fun () -> loop (n + 1)
          | e ->
              Alcotest.failf "Cannot connect to server %a: %a" pp id Fmt.exn e)
  in
  loop 1

let wait_for_the_server_to_start id =
  let rec aux n =
    let pid_file = pid_file id in
    let socket = socket id in
    if Sys.file_exists pid_file && Sys.file_exists socket then (
      let ic = open_in pid_file in
      let line = input_line ic in
      close_in ic;
      let pid = int_of_string line in
      Logs.debug (fun l -> l "read PID %d from %s" pid pid_file);
      Unix.unlink pid_file;
      check_connection id >|= fun () -> pid)
    else (
      Logs.debug (fun l -> l "waiting for the server to start...");
      Lwt_unix.sleep (float n *. 0.1) >>= fun () -> aux (n + 1))
  in
  aux 1

let servers = [ (`Quick, Test_mem.suite); (`Quick, Test_git.suite) ]

module Conf = Irmin_http.Conf

let root c = Conf.(get c (root ()))

let mkdir d =
  Lwt.catch
    (fun () -> Lwt_unix.mkdir d 0o755)
    (function
      | Unix.Unix_error (Unix.EEXIST, _, _) -> Lwt.return_unit | e -> Lwt.fail e)

let rec lock id =
  let pid = string_of_int (Unix.getpid ()) in
  let pid_len = String.length pid in
  let pid_file = pid_file id in
  let pid_file_tmp = tmp_file pid_file in
  (* [fd] is used to write the actual PID file; the file is renamed
     bellow to ensure atomicity. *)
  let* fd =
    Lwt_unix.openfile pid_file_tmp [ Unix.O_CREAT; Unix.O_RDWR ] 0o600
  in
  Lwt.catch
    (fun () ->
      Lwt_unix.lockf fd Unix.F_LOCK 0 >>= fun () ->
      Logs.debug (fun l -> l "write PID %s in %s" pid pid_file);
      let* len = Lwt_unix.write fd (Bytes.of_string pid) 0 pid_len in
      if len <> pid_len then
        Lwt_unix.close fd >>= fun () ->
        Lwt.fail_with "Unable to write PID to lock file"
      else Lwt_unix.rename pid_file_tmp pid_file >|= fun () -> fd)
    (function
      | Unix.Unix_error (Unix.EAGAIN, _, _) ->
          Lwt_unix.close fd >>= fun () -> lock id
      | e -> Lwt_unix.close fd >>= fun () -> Lwt.fail e)

let unlock fd = Lwt_unix.close fd

let serve servers n id =
  Logs.set_level ~all:true (Some Logs.Debug);
  Logs.debug (fun l -> l "pwd: %s" @@ Unix.getcwd ());
  let _, (server : Irmin_test.t) = List.nth servers n in
  Logs.debug (fun l ->
      l "Got server: %s, root=%s" server.name (root server.config));
  let (module Server : Irmin_test.S) = server.store in
  let module HTTP = Irmin_http.Server (Cohttp_lwt_unix.Server) (Server) in
  let test = { name = server.name; id } in
  let socket = socket test in
  let server () =
    server.init () >>= fun () ->
    let* repo = Server.Repo.v server.config in
    let* lock = lock test in
    let spec = HTTP.v repo ~strict:false in
    let* () =
      Lwt.catch
        (fun () -> Lwt_unix.unlink socket)
        (function Unix.Unix_error _ -> Lwt.return_unit | e -> Lwt.fail e)
    in
    let mode = `Unix_domain_socket (`File socket) in
    Conduit_lwt_unix.set_max_active 100;
    let* () =
      Cohttp_lwt_unix.Server.create
        ~on_exn:(Fmt.pr "Async exception caught: %a" Fmt.exn)
        ~mode spec
    in
    unlock lock
  in
  Lwt_main.run (server ())

let kill_server socket pid =
  let () =
    try
      Unix.kill pid Sys.sigkill;
      try ignore (Unix.waitpid [ Unix.WUNTRACED ] pid) with _ -> ()
    with Unix.Unix_error (Unix.ESRCH, _, _) -> ()
  in
  Unix.unlink socket;
  Fmt.epr "Server [PID %d] is killed.\n%!" pid

let suite i server =
  let open Irmin_test in
  let id = { name = server.name; id = Random.int 0x3FFFFFFF } in
  let socket = socket id in
  let server_pid = ref 0 in
  {
    name = Printf.sprintf "HTTP.%s" server.name;
    init =
      (fun () ->
        remove socket;
        remove (pid_file id);
        mkdir test_http_dir >>= fun () ->
        Lwt_io.flush_all () >>= fun () ->
        let pwd = Sys.getcwd () in
        let root =
          if Filename.basename pwd = "default" then ".." / ".." / "" else ""
        in
        let cmd =
          root
          ^ "_build"
            / "default"
            / Fmt.strf "%s serve %d %d &" Sys.argv.(0) i id.id
        in
        Fmt.epr "pwd=%s\nExecuting: %S\n%!" pwd cmd;
        let _ = Sys.command cmd in
        let+ pid = wait_for_the_server_to_start id in
        server_pid := pid);
    stats = None;
    clean =
      (fun () ->
        kill_server socket !server_pid;
        server.clean ());
    config = Irmin_http.config uri Conf.empty;
    store = http_store id server.store;
    layered_store = None;
  }

let suites servers =
  if Sys.os_type = "Win32" then
    (* it's a bit hard to test client/server stuff on windows because
       we can't fork. Can work around that later if needed. *)
    []
  else List.mapi (fun i (s, server) -> (s, suite i server)) servers

let with_server servers f =
  if Array.length Sys.argv = 4 && Sys.argv.(1) = "serve" then (
    let n = int_of_string Sys.argv.(2) in
    let id = int_of_string Sys.argv.(3) in
    Logs.set_reporter (Irmin_test.reporter ~prefix:"S" ());
    serve servers n id)
  else f ()

type test = Alcotest.speed_level * Irmin_test.t
