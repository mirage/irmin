(*
 * Copyright (c) 2013-2019 Thomas Gazagnaire <thomas@gazagnaire.org>
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

let config ~root = Irmin_pack.config ~fresh:false root

module Config = struct
  let entries = 2

  let stable_hash = 3
end

module KV = Irmin_pack.KV (Config) (Irmin.Contents.String)
module Bench = Irmin_bench.Make (KV)

let file f =
  (* in MiB *)
  try (Unix.stat f).st_size with Unix.Unix_error (Unix.ENOENT, _, _) -> 0

let dict root = file (Filename.concat root "store.dict") / 1024 / 1024

let index root =
  let rec aux acc i =
    if i = 256 then acc
    else
      let filename = Format.sprintf "store.index.%d" i in
      let s = file (Filename.concat root filename) in
      aux (acc + s) (i + 1)
  in
  aux 0 0 / 1024 / 1024

let log root = file (Filename.concat root "store.log")

let pack root = file (Filename.concat root "store.pack") / 1024 / 1024

let size ~root = dict root + index root + pack root + log root

let () = Bench.run ~config ~size
