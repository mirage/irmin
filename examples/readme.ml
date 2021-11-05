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

(* N.B. This excerpt is extracted from project README. Any changes made here
 * should be mirrored there. *)

open Lwt.Syntax

(* Irmin store with string contents *)
module Store = Irmin_unix.Git.FS.KV (Irmin.Contents.String)

(* Database configuration *)
let config = Irmin_git.config ~bare:true "/tmp/irmin/test"

(* Commit author *)
let author = "Example <example@example.com>"

(* Commit information *)
let info fmt = Irmin_unix.info ~author fmt

let main =
  (* Open the repo *)
  let* repo = Store.Repo.v config in

  (* Load the main branch *)
  let* t = Store.main repo in

  (* Set key "foo/bar" to "testing 123" *)
  let* () =
    Store.set_exn t ~info:(info "Updating foo/bar") [ "foo"; "bar" ]
      "testing 123"
  in

  (* Get key "foo/bar" and print it to stdout *)
  let+ x = Store.get t [ "foo"; "bar" ] in
  Printf.printf "foo/bar => '%s'\n" x

(* Run the program *)
let () = Lwt_main.run main
