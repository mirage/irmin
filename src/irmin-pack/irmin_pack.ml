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

include Ext
include Config

let config = Config.v

module Pack = Pack
module Atomic_write = Store.Atomic_write
module Hash = Irmin.Hash.BLAKE2B
module Path = Irmin.Path.String_list
module Metadata = Irmin.Metadata.None
module Make_ext = Ext.Make

module Make
    (Config : Config.S)
    (M : Irmin.Metadata.S)
    (C : Irmin.Contents.S)
    (P : Irmin.Path.S)
    (B : Irmin.Branch.S)
    (H : Irmin.Hash.S) =
struct
  module XNode = Irmin.Private.Node.Make (H) (P) (M)
  module XCommit = Irmin.Private.Commit.Make (H)
  include Make_ext (Config) (M) (C) (P) (B) (H) (XNode) (XCommit)
end

module KV (Config : Config.S) (C : Irmin.Contents.S) =
  Make (Config) (Metadata) (C) (Path) (Irmin.Branch.String) (Hash)
module Stats = Stats
module Layout = Layout
module Checks = Checks

module Private = struct
  module Utils = Utils
end

module Layered = Irmin_pack_layers

let config_layers = Irmin_pack_layers.config_layers

module Config = Config
