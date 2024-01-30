(*
 * Copyright (c) 2013-2022 Thomas Gazagnaire <thomas@gazagnaire.org>
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

let info = Irmin_git_unix.info

let path =
  if Array.length Sys.argv = 2 then Sys.argv.(1)
  else "git://github.com/mirage/ocaml-git.git"

module Store = Irmin_git_unix.FS.KV (Irmin.Contents.String)
module Sync = Irmin.Sync.Make (Store)

let test () =
  Eio.Switch.run @@ fun sw ->
  Config.init ();
  let config = Irmin_git.config Config.root in
  let repo = Store.Repo.v ~sw config in
  let t = Store.of_branch repo "master" in
  let upstream = Store.remote path () in
  let _ = Sync.pull_exn t upstream `Set in
  let readme = Store.get t [ "README.md" ] in
  let tree = Store.get_tree t [] in
  let tree = Store.Tree.add tree [ "BAR.md" ] "Hoho!" in
  let tree = Store.Tree.add tree [ "FOO.md" ] "Hihi!" in
  Store.set_tree_exn t ~info:(info "merge") [] tree;
  Printf.printf "%s\n%!" readme

let () =
  Eio_main.run @@ fun env ->
  Lwt_eio.with_event_loop ~clock:env#clock @@ fun _ -> test ()
