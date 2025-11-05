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

let stats () =
  let stats = Irmin_watcher.stats () in
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

let init ~config =
  clean_dirs config;
  Irmin.Backend.Watch.set_listen_dir_hook Irmin_watcher.hook

let clean ~config =
  clean_dirs config;
  Irmin.Backend.Watch.(set_listen_dir_hook none)

let suite ~path ~clock =
  let config = Irmin_fs_unix.conf ~path ~clock in
  Irmin_test.Suite.create ~name:"FS.UNIX" ~init ~store ~config ~clean ~stats ()
