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

open Test_common
open Lwt.Infix

let stats =
  Some (fun () ->
      let stats = Irmin_watcher.stats () in
      stats.Irmin_watcher.watchdogs, Irmin.Private.Watch.workers ()
    )

(* FS *)

module FS = struct

  let test_db = Test_fs.test_db
  let config = Test_fs.config
  let store = store (module Irmin_unix.FS.Make) (module Irmin.Metadata.None)

  let init () =
    if Sys.file_exists test_db then begin
      let cmd = Printf.sprintf "rm -rf %s" test_db in
      let _ = Sys.command cmd in ()
    end;
    Irmin_unix.set_listen_dir_hook ();
    Lwt.return_unit

  let clean () =
    Irmin.Private.Watch.(set_listen_dir_hook none);
    Lwt.return_unit

  let suite = { name = "FS"; kind = `Unix; clean; init; store; stats; config }

  module Link = struct
    include Irmin_unix.FS.Link(Irmin.Hash.SHA1)
    let v () = v (Irmin_fs.config test_db)
  end

  let link = (module Link: Test_link.S)

end

(* GIT *)

module Git = struct

  let test_db = Test_git.test_db
  let init () =
    (if Sys.file_exists ".git" then
      failwith "The Git test should not be run at the root of a Git repository."
    else if Sys.file_exists test_db then
      Git_unix.FS.create ~root:test_db () >>= fun t ->
      Git_unix.FS.reset t
    else
      Lwt.return_unit)
    >|= fun () ->
    Irmin_unix.set_listen_dir_hook ()

  module S = struct
    module S = Irmin_unix.Git.FS.KV(Irmin.Contents.String)
    let author repo c =
      S.Git.git_commit repo c >|= function
      | None   -> None
      | Some c -> Some c.Git.Commit.author.Git.User.name
    include S
    let init = init
  end
  let store = (module S: Test_git.Test_S)

  let clean () =
    Irmin.Private.Watch.(set_listen_dir_hook none);
    Lwt.return_unit

  let config =
    let head = Git.Reference.of_raw "refs/heads/test" in
    Irmin_git.config ~head ~bare:true test_db

  let suite =
    let store = (module S: Test_S) in
    { name = "GIT"; kind = `Unix; clean; init; store; stats; config }

  let test_non_bare () =
    init () >>= fun () ->
    let config = Irmin_git.config ~bare:false test_db in
    let info = Irmin_unix.info in
    S.Repo.v config >>= fun repo ->
    S.master repo >>= fun t ->
    S.set t ~info:(info "fst one") ["fst"] "ok" >>= fun () ->
    S.set t ~info:(info "snd one") ["fst"; "snd"] "maybe?" >>= fun () ->
    S.set t ~info:(info "fst one") ["fst"] "hoho"

  let misc = "non-bare", `Quick, (fun () -> Lwt_main.run (test_non_bare ()))

end

module Http = struct

  let servers = [
    `Quick, FS.suite;
    `Quick, Git.suite;
  ]

end
