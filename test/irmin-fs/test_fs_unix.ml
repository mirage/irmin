(*
 * Copyright (c) 2022 Tarides <contact@tarides.com>
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

let stats watcher () =
  let stats = Irmin_watcher.stats watcher in
  (stats.Irmin_watcher.watchdogs, Irmin.Backend.Watch.workers ())

let test_db = Test_fs.test_db
let store = Irmin_test.store (module Irmin_fs_unix) (module Irmin.Metadata.None)

let clean_dirs config =
  let test_db =
    Irmin.Backend.Conf.find_root config |> Option.value ~default:test_db
  in
  if Sys.file_exists test_db then
    let cmd = Printf.sprintf "rm -rf %s" test_db in
    let _ = Sys.command cmd in
    ()

let init ~sw ~fs ~watcher ~config =
  clean_dirs config;
  Irmin.Backend.Watch.set_listen_dir_eio_hook ~sw fs
    (Irmin_watcher.hook watcher)

let clean ~config =
  clean_dirs config;
  Irmin.Backend.Watch.(set_listen_dir_hook none)

let suite ~sw ~fs ~watcher ~path ~clock =
  let config = Irmin_fs_unix.config ~root:path ~clock in
  Irmin_test.Suite.create ~name:"FS.UNIX" ~init:(init ~sw ~fs ~watcher) ~store
    ~config ~clean ~stats:(stats watcher) ()

module W =
  Irmin.Backend.Watch.Make
    (struct
      type t = string

      let t = Irmin.Type.string
    end)
    (struct
      type t = string

      let t = Irmin.Type.string
    end)

(* A file written in a watched directory, outside of Irmin, is reported
   through the listen hook, with its path relative to the directory. *)
let test_listen_dir ~sw ~fs ~watcher ~clock () =
  Irmin.Backend.Watch.set_listen_dir_eio_hook ~sw fs
    (Irmin_watcher.hook watcher);
  let dir = Eio.Path.(fs / "test-listen-dir") in
  Eio.Path.rmtree ~missing_ok:true dir;
  Eio.Path.mkdir ~perm:0o700 dir;
  Fun.protect ~finally:(fun () ->
      Irmin.Backend.Watch.(set_listen_dir_hook none);
      Eio.Path.rmtree ~missing_ok:true dir)
  @@ fun () ->
  let value key =
    try Some (Eio.Path.load Eio.Path.(dir / key))
    with Eio.Io (Eio.Fs.E (Eio.Fs.Not_found _), _) -> None
  in
  let workers = Irmin.Backend.Watch.workers () in
  let w = W.v () in
  let stop = W.listen_dir w "test-listen-dir" ~key:Option.some ~value in
  let changes = Eio.Stream.create max_int in
  let id = W.watch_key w "foo" (Eio.Stream.add changes) in
  ( Fun.protect ~finally:(fun () ->
        W.unwatch w id;
        stop ())
  @@ fun () ->
    (* Write [foo] atomically, so that it is never read empty. *)
    Eio.Path.save ~create:(`Exclusive 0o600) Eio.Path.(dir / "foo.tmp") "bar";
    Eio.Path.rename Eio.Path.(dir / "foo.tmp") Eio.Path.(dir / "foo");
    match
      Eio.Time.with_timeout clock 60. (fun () -> Ok (Eio.Stream.take changes))
    with
    | Ok (`Added "bar") -> ()
    | Ok _ -> Alcotest.fail "foo should be added with the value bar"
    | Error `Timeout -> Alcotest.fail "no change reported for foo within 60s" );
  (* The notification worker stops once the watch is removed. *)
  let rec stopped () =
    if Irmin.Backend.Watch.workers () <= workers then Ok ()
    else (
      Eio.Time.sleep clock 0.01;
      stopped ())
  in
  match Eio.Time.with_timeout clock 10. stopped with
  | Ok () -> ()
  | Error `Timeout -> Alcotest.fail "the notification worker did not stop"

let misc ~sw ~fs ~watcher ~clock =
  [ ("listen hook", `Quick, test_listen_dir ~sw ~fs ~watcher ~clock) ]
