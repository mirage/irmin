(*
 * Copyright (c) 2018-2022 Tarides <contact@tarides.com>
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

module Config = struct
  let entries = 2
  let stable_hash = 3
  let contents_length_header = Some `Varint
  let inode_child_order = `Hash_bits
  let forbid_empty_dir_persistence = false
end

module KV = struct
  module Maker = Irmin_pack_unix.KV (Config)
  include Maker.Make (Irmin.Contents.String)
end

module Bench = Irmin_bench.Make (KV)

let file f =
  (* in MiB *)
  try
    Eio.Switch.run @@ fun sw ->
    let open Optint.Int63 in
    let f = Eio.Path.open_in ~sw f in
    Infix.(to_int @@ (Eio.File.size f / of_int 1024 / of_int 1024))
  with Eio.Exn.Io (Eio.Fs.E (Not_found _), _) -> 0

let index root =
  let rec aux acc i =
    if i = 256 then acc
    else
      let filename = Format.sprintf "store.index.%d" i in
      let s = file Eio.Path.(root / filename) in
      aux (acc + s) (i + 1)
  in
  aux 0 0

let size ~root =
  let index_size = index root in
  Irmin_pack.Layout.V1_and_v2.all ~root
  |> List.map file
  |> List.fold_left ( + ) index_size

let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let fs = Eio.Stdenv.cwd env in
  let config ~root =
    Irmin_pack.config ~sw ~fs ~fresh:false Eio.Path.(fs / root)
  in
  let size ~root = size ~root:Eio.Path.(fs / root) in
  Bench.run ~config ~size
