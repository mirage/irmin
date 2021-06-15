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

module Hash = Irmin.Hash.BLAKE2B
module Path = Irmin.Path.String_list
module Metadata = Irmin.Metadata.None
module Node = Irmin.Private.Node.Make
module Commit = Irmin.Private.Commit

module Conf = struct
  let entries = 32
  let stable_hash = 256
end

module Maker (V : Irmin_pack.Version.S) = struct
  module Maker = Irmin_pack.Maker_ext (V) (Conf) (Node) (Commit)

  include
    Maker.Make (Irmin.Metadata.None) (Irmin.Contents.String) (Path)
      (Irmin.Branch.String)
      (Hash)
end

module Store = Irmin_pack.Checks.Make (Maker)

module S =
  Irmin_pack_layered.Maker_ext (Conf) (Node) (Commit) (Irmin.Metadata.None)
    (Irmin.Contents.String)
    (Path)
    (Irmin.Branch.String)
    (Hash)

module Store_layered = Irmin_pack_layered.Checks.Make (Maker) (S)

let () =
  match Sys.getenv_opt "PACK_LAYERED" with
  | Some "true" -> ( match Store_layered.cli () with _ -> .)
  | _ -> ( match Store.cli () with _ -> .)
