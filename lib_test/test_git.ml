(*
 * Copyright (c) 2013 Thomas Gazagnaire <thomas@gazagnaire.org>
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

open Lwt
open Test_common

let test_db = "test_db_git"

let init_disk () =
  if Filename.basename (Sys.getcwd ()) <> "lib_test" then
    failwith "The Git test should be run in the lib_test/ directory."
  else if Sys.file_exists test_db then
    Git_unix.FS.create ~root:test_db () >>= fun t ->
    Irmin_unix.install_dir_polling_listener Test_fs.polling;
    Git_unix.FS.remove t
  else
    return_unit

let clean () =
  Irmin_unix.uninstall_dir_polling_listener ();
  Lwt.return_unit

let suite k =
  {
    name   = "GIT" ^ string_of_contents k;
    cont   = k;
    kind   = `Git;
    init   = init_disk;
    clean  = clean;
    store  = git_store k;
    config =
      let head = Git.Reference.of_raw "refs/heads/test" in
      Irmin_git.config ~root:test_db ~head ~bare:true ()
  }

let test_non_bare () =
  let open Irmin_unix in
  init_disk () >>= fun () ->
  let module Store = Irmin_git.FS(Irmin.Contents.String)(Irmin.Ref.String)(Irmin.Hash.SHA1) in
  let config = Irmin_git.config ~root:test_db ~bare:false () in
  Store.Repo.create config >>= Store.master task >>= fun t ->
  Store.update (t "fst one") ["fst"] "ok" >>= fun () ->
  Store.update (t "snd one") ["fst"; "snd"] "maybe?" >>= fun () ->
  Store.update (t "fst one") ["fst"] "hoho"

let run f () = Lwt_main.run (f ())

let misc =
  "GIT.misc", [
    "Testing git non-bare repostiories", `Quick, run test_non_bare;
  ]
