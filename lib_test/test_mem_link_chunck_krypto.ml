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

let test_db = "test-db-chunck"

let polling =
  try float_of_string (Sys.getenv "IRMIN_FS_POLLING")
  with Not_found | Failure _ -> 0.01

let init () =
  Irmin_unix.install_dir_polling_listener polling;
  if Sys.file_exists test_db then begin
    let cmd = Printf.sprintf "rm -rf %s" test_db in
    let _ = Sys.command cmd in ()
  end;
  Lwt.return_unit

let clean () =
  Irmin_unix.uninstall_dir_polling_listener ();
  Lwt.return_unit
    
let config = Irmin_fs.config ~root:test_db ()
let config = Irmin_chunck.config ~conf:config ~size:4096 ()
    
let suite k =
  {
    name   = "MEM-LINK.CHUNCK.KRYPTO" ^ string_of_contents k;
    kind   = `Link_MEM_Chunck_Krypto;
    cont   = k;
    init   = init;
    clean  = clean;
    store  = link_mem_chunck_krypto_store k;
    config = config;
  }
