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

include Ext
include Irmin_pack_intf

let config = Conf.v

exception RO_not_allowed = S.RO_not_allowed

module Content_addressable = Content_addressable
module Atomic_write = Atomic_write
module Dict = Pack_dict
module Hash = Irmin.Hash.BLAKE2B
module Path = Irmin.Path.String_list
module Metadata = Irmin.Metadata.None
module Version = Version
module Index = Pack_index
module Conf = Conf
module Schema = Schema

let migrate = Migrate.run

module Make = Ext.Make

module V1 (S : Schema.Unversioned) = Make (struct
  include S
  module Version = Version.V1
end)

module V2 (S : Schema.Unversioned) = Make (struct
  include S
  module Version = Version.V2
end)

module KV (Version : Version.S) (Config : Conf.S) (C : Irmin.Contents.S) =
Make (struct
  include Irmin.Schema.KV (C)
  module Version = Version
  module Config = Config
end)

module Stats = Stats
module Layout = Layout
module Checks = Checks
module Inode = Inode
module IO = IO
module Utils = Utils
module Pack_value = Pack_value
module Pack_store = Pack_store
