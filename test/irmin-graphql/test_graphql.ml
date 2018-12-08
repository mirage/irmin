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

module Store = Irmin_mem.KV(Irmin.Contents.String)
module Client = Irmin_unix.Graphql.Client.Make(Store)

(* See https://github.com/mirage/ocaml-cohttp/issues/511 *)
let () = Lwt.async_exception_hook := (fun e ->
    Fmt.pr "Async exception caught: %a" Fmt.exn e;
  )

let unwrap = function
  | Ok x -> Lwt.return x
  | Error (`Msg e) -> Alcotest.fail e

let test_set_and_get client =
  Client.set client ["a"; "b"; "c"] "123" >>= unwrap >>= fun _ ->
  Client.get client ["a"; "b"; "c"] >>= unwrap >|= fun s ->
  Alcotest.(check string) "get a/b/c" "123" s

let uri = Uri.of_string "http://localhost:80808/graphql"

let run_tests name tests =
  let client = Irmin_unix.Graphql.Client.init uri in
  let tests = List.map (fun (name, speed, f) ->
      name, speed, (fun () -> Lwt_main.run (f client))) tests
  in
  Alcotest.run name [name, tests]

let server_pid = ref 0

let clean () =
  Unix.kill !server_pid Sys.sigint

let run_server () =
  match Lwt_unix.fork () with
  | -1 ->
    failwith "unable to fork process"
  | 0 ->
    let newstdout = open_out "server.log" in
    let fd = Unix.descr_of_out_channel newstdout |> Lwt_unix.of_unix_file_descr in
    Lwt_unix.dup2 fd Lwt_unix.stdout;
    Lwt_unix.dup2 fd Lwt_unix.stderr;
    let () = Logs.set_reporter (Irmin_test.reporter ~prefix:"S" ()) in
    Logs.set_level ~all:true (Some Logs.Debug);
    Logs.debug (fun l -> l "pwd: %s" @@ Unix.getcwd ());
    let module Server = Irmin_unix.Graphql.Server.Make(Store)(struct let remote = None end) in
    let server () =
      Lwt_unix.on_signal Sys.sigint (fun _ -> exit 0) |> ignore;
      Store.Repo.v (Irmin_mem.config ()) >>= Store.master >>= fun t ->
      server_pid := Unix.getpid ();
      Conduit_lwt_unix.set_max_active 100;
      Server.run_server (None, `TCP (`Port 80808)) t
    in
    Lwt_main.run (server ())
  | n ->
      Sys.signal Sys.sigint (Sys.Signal_handle (fun _ -> clean ())) |> ignore ;
      server_pid := n

let run () =
  if Sys.os_type = "Win32" then
    (* it's a bit hard to test client/server stuff on windows because
       we can't fork. Can work around that later if needed. *)
    exit 0
  else
    let () = run_server () in
    Unix.sleep 2;
    let () = run_tests "GRAPHQL" [
        "Set and get", `Quick, test_set_and_get;
      ] in
    clean ()
