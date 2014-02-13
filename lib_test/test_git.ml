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
open Test_store

let test_db = ".git"

let init_memory () =
  Git_memory.create () >>= fun t ->
  Git_memory.clear t

let init_local () =
  if Filename.basename (Sys.getcwd ()) <> "lib_test" then
    failwith "The Git test should be run in the lib_test/ directory."
  else if Sys.file_exists test_db then
    Git_fs.create ~root:"." () >>= fun t ->
    Git_fs.clear t
  else
    return_unit

let init = function
  | `Local  -> init_local
  | `Memory -> init_memory

let string_of_g = function
  | `Local  -> ""
  | `Memory -> ".MEM"

let suite k g =
  {
    name  = "GIT" ^ string_of_g g ^ string_of_kind k;
    kind  = k;
    init  = init g;
    clean = unit;
    store = IrminGit.create k g;
  }
