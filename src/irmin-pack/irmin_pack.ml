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

let config = Conf.make

exception RO_not_allowed = S.RO_not_allowed

module type S = S.S

module Content_addressable = Content_addressable
module Atomic_write = Atomic_write
module Dict = Pack_dict
module Hash = Irmin.Hash.BLAKE2B
module Path = Irmin.Path.String_list
module Metadata = Irmin.Metadata.None
module Maker_ext = Ext.Maker
module Version = Version
module Index = Pack_index
module Conf = Conf

let migrate = Migrate.run

module Maker = Maker_ext
module V1 = Maker (Version.V1)
module V2 = Maker (Version.V2)

module KV (V : Version.S) (Config : Conf.S) = struct
  type endpoint = unit

  module Maker = Maker (V) (Config)

  type metadata = Metadata.t

  module Make (C : Irmin.Contents.S) = Maker.Make (Irmin.Schema.KV (C))
end

module Stats = Stats
module Layout = Layout
module Checks = Checks
module Inode = Inode
module IO = IO
module Utils = Utils
module Pack_value = Pack_value
module Pack_store = Pack_store
module Vx = Version.V1

module Cx = struct
  let stable_hash = 0
  let entries = 0
end

(* Enforce that {!KV} is a sub-type of {!Irmin.KV_maker}. *)
module KV_is_a_KV_maker : Irmin.KV_maker = KV (Vx) (Cx)

(* Enforce that {!Maker} is a sub-type of {!Irmin.Maker}. *)
module Maker_is_a_maker : Irmin.Maker = Maker (Vx) (Cx)
