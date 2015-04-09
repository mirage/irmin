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

open Test_common

let test_db = "test-db"

let init () =
  Irmin_unix.install_dir_polling_listener 0.1;
  if Sys.file_exists test_db then begin
    let cmd = Printf.sprintf "rm -rf %s" test_db in
    let _ = Sys.command cmd in ()
  end;
  Lwt.return_unit

let clean () =
  Irmin_unix.uninstall_dir_polling_listener ();
  Lwt.return_unit

let suite k =
  {
    name = "FS" ^ string_of_kind k;
    kind = k;
    disk = true;
    init; clean;
    config = Irmin_fs.config ~root:test_db ();
    store  =  irf_store k;
  }
