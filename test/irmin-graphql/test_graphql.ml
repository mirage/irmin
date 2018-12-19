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

let ( / ) = Filename.concat
let pid_file = Filename.get_temp_dir_name () / "irmin-graphql-test.pid"

let rec wait_for_the_server_to_start () =
  if Sys.file_exists pid_file then (
    let ic = open_in pid_file in
    let line = input_line ic in
    close_in ic;
    let pid = int_of_string line in
    Logs.debug (fun l -> l "read PID %d fomr %s" pid pid_file);
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

let test_set_and_get branch client =
  Client.set ~branch client ["a"; "b"; "c"] "123" >>= unwrap >>= fun _ ->
  Client.get ~branch client ["a"; "b"; "c"] >>= unwrap >>= fun s ->
  Client.set ~branch client ["foo"] "bar" >>= unwrap >>= fun _ ->
  Client.get ~branch client ["foo"] >>= unwrap >|= fun s' ->
  Alcotest.(check string) "get a/b/c" "123" s;
  Alcotest.(check string) "get foo" "bar" s'

let test_head branch client =
  Client.branch_info client branch >>= unwrap >>= fun info ->
  let hash = info.Client.hash in
  Client.commit_info client hash >>= unwrap >>= fun info' ->
  check_type_eq "Commit info" Irmin.Info.t info.info info'.info;
  Client.set ~branch client ["foo"] "baz" >>= unwrap >|= fun hash' ->
  check_type_not_eq "hash after set" Store.Hash.t hash hash'

let test_remove branch client =
  Client.branch_info client branch >>= unwrap >>= fun info ->
  let hash = info.Client.hash in
  Client.remove client ["foo"] >>= unwrap >>= fun hash' ->
  check_type_not_eq "hash after remove" Store.Hash.t hash hash';
  Client.find client ["foo"] >>= unwrap >|= function
  | Some _ -> Alcotest.fail "foo should be empty"
  | None -> ()

let test_tree branch client =
  Client.branch_info client branch >>= unwrap >>= fun info ->
  let hash = info.Client.hash in
  let tree = Store.Tree.empty in
  Store.Tree.add tree ["test"; "a"] "1" >>= fun tree ->
  Store.Tree.add tree ["test"; "b"] "2" >>= fun tree ->
  Store.Tree.add tree ["test"; "c"] "3" >>= fun tree ->
  Client.update_tree ~branch client [] tree >>= unwrap >|= fun _hash' ->
    let _ = hash in
  (*check_type_not_eq "hash after update tree" Store.Hash.t hash hash'*) ()

let tests = [
  "set/get", `Quick, test_set_and_get;
  "branch_info/commit_info", `Quick, test_head;
  "remove", `Quick, test_remove;
  "tree", `Quick, test_tree;
]

let uri = Uri.of_string "http://localhost:80808/graphql"

let run_tests name tests =
  let client = Client.init uri in
  let tests branch =
    List.map (fun (name, speed, f) ->
        branch ^ ":" ^ name, speed, (fun () -> Lwt_main.run (Lwt_unix.on_signal Sys.sigint (fun _ -> exit 0) |> ignore; f branch client))) tests
  in
  let a = tests "master" in
  let b = tests "testing" in
  Alcotest.run name [name, a @ b]

let server_pid = ref 0

let clean () =
  Unix.sleep 1;
  Unix.kill !server_pid Sys.sigint

let run_server () =
  let module Server = Irmin_unix.Graphql.Server.Make(Store)(struct let remote = None end) in
  let server =
    Lwt_unix.on_signal Sys.sigint (fun _ -> exit 0) |> ignore;
    Store.Repo.v (Irmin_mem.config ()) >>= Store.master >>= fun t ->
    server_pid := Unix.getpid ();
    Conduit_lwt_unix.set_max_active 100;
    let server = Server.server t in
    Cohttp_lwt_unix.Server.create ~mode:(`TCP (`Port 80808)) server
  in
  Lwt_main.run server

let run () =
  if Sys.os_type = "Win32" then
    (* it's a bit hard to test client/server stuff on windows because
       we can't fork. Can work around that later if needed. *)
    exit 0
  else
  if (Array.length Sys.argv > 1 && Sys.argv.(1) = "server") then
    run_server ()
  else
    let () = at_exit clean in
    let _ = Sys.command (Printf.sprintf "dune exec -- %s server & echo $! > %s" Sys.argv.(0) pid_file) in
    let () = server_pid := wait_for_the_server_to_start () in
    run_tests "GRAPHQL" tests
