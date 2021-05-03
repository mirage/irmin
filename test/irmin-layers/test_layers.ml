(*
 * Copyright (c) 2018-2021 Tarides <contact@tarides.com>
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

open! Import

module Config = struct
  let entries = 2
  let stable_hash = 3
end

module Maker = Irmin_layers_pack.Maker (Config)

let test_dir = Filename.concat "_build" "test-db-layers"

let config =
  let conf = Irmin_pack.config ~fresh:true ~lru_size:0 test_dir in
  Irmin_layers_pack.config ~conf ~with_lower:true ~blocking_copy_size:1000 ()

let clean () = Lwt.return ()

let init () =
  if Sys.file_exists test_dir then (
    let cmd = Printf.sprintf "rm -rf %s" test_dir in
    Fmt.epr "exec: %s\n%!" cmd;
    let _ = Sys.command cmd in
    ());
  Lwt.return_unit

module Store = struct
  include
    Maker.Make (Irmin.Metadata.None) (Irmin.Contents.String)
      (Irmin.Path.String_list)
      (Irmin.Branch.String)
      (Irmin.Hash.BLAKE2B)

  let gc_hook = Some (fun repo max -> freeze repo ~max_lower:max)
end

let suite =
  let store = (module Store : Irmin_test.S) in
  { Irmin_test.name = "LAYERS"; init; clean; config; store; stats = None }

module TL = Layered_store.Make (Store)

let misc =
  let t = { suite with store = (module Store : Irmin_test.S) } in
  let run f = Lwt_main.run (init () >|= fun () -> f t) in
  [ ("MISC", TL.suite run) ]
