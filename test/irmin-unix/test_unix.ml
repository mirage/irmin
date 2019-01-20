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

let stats =
  Some (fun () ->
      let stats = Irmin_watcher.stats () in
      stats.Irmin_watcher.watchdogs, Irmin.Private.Watch.workers ()
    )

(* FS *)

module FS = struct

  let test_db = Test_fs.test_db
  let config = Test_fs.config
  let store =
    Irmin_test.store (module Irmin_unix.FS.Make) (module Irmin.Metadata.None)

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

  let suite =
    { Irmin_test.name = "FS"; clean; init; store; stats; config }

end

(* GIT *)

module Git = struct

  let test_db = Test_git.test_db
  let init () =
    if Sys.file_exists ".git" then (
      Fmt.epr "The Git test should not be run at the root of a Git repository, \
               changing to a subdir...\n%!";
      Unix.chdir Fpath.(to_string @@ v "test" / "irmin-unix")
    );
    (if Sys.file_exists test_db then
       Git_unix.Store.v (Fpath.v test_db) >>= function
       | Ok t    -> Git_unix.Store.reset t >|= fun _ -> ()
       | Error _ -> Lwt.return ()
     else
       Lwt.return_unit)
    >|= fun () ->
    Irmin_unix.set_listen_dir_hook ()

  module S = struct
    module G = struct
      include Git_unix.Store
      let v ?dotgit ?compression ?buffers root =
        let buffer = match buffers with
          | None   -> None
          | Some p -> Some (Lwt_pool.use p)
        in
        v ?dotgit ?compression ?buffer root
    end
    module S = Irmin_unix.Git.FS.KV(Irmin.Contents.String)
    let author repo c =
      S.git_commit repo c >|= function
      | None   -> None
      | Some c -> Some (S.Git.Value.Commit.author c).Git.User.name
    include S
    let init = init
  end
  let store = (module S: Test_git.Test_S)

  let clean () =
    Irmin.Private.Watch.(set_listen_dir_hook none);
    Lwt.return_unit

  let config =
    let head = Git.Reference.of_string "refs/heads/test" in
    Irmin_git.config ~head ~bare:true test_db

  let suite =
    let store = (module S: Irmin_test.S) in
    { Irmin_test.name = "GIT"; clean; init; store; stats; config }

  let test_non_bare () =
    init () >>= fun () ->
    let config = Irmin_git.config ~bare:false test_db in
    let info = Irmin_unix.info in
    S.Repo.v config >>= fun repo ->
    S.master repo >>= fun t ->
    S.set_exn t ~info:(info "fst one") ["fst"] "ok" >>= fun () ->
    S.set_exn t ~info:(info "snd one") ["fst"; "snd"] "maybe?" >>= fun () ->
    S.set_exn t ~info:(info "fst one") ["fst"] "hoho"

  let misc = "non-bare", `Quick, (fun () -> Lwt_main.run (test_non_bare ()))

end

module Http = struct

  let servers = [
    `Quick, FS.suite;
    `Quick, Git.suite;
  ]

end
