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

(* include Ext (\* single Maker functor; rebound to Maker_ext below, then rebound again to Maker; so not actually needed *\) *)
include Irmin_pack_intf

let config = Conf.init

exception RO_not_allowed = S.RO_not_allowed

module Indexable = Indexable
module Atomic_write = Atomic_write
module Dict = Pack_dict
module Hash = Irmin.Hash.BLAKE2B
module Path = Irmin.Path.String_list
module Metadata = Irmin.Metadata.None
module Maker_ext = Ext.Maker
module Version = Version
module Index = Pack_index
module Conf = Conf
module Maker = Maker_ext

module KV (Config : Conf.S) = struct
  type endpoint = unit
  type hash = Irmin.Schema.default_hash

  include Pack_key.Store_spec
  module Maker = Maker (Config)

  type metadata = Metadata.t

  module Make (C : Irmin.Contents.S) = Maker.Make (Irmin.Schema.KV (C))
end

module Stats = Stats
module Layout = Layout
module Checks = Checks
module Inode = Inode
module IO = IO
module Pack_key = Pack_key
module Pack_value = Pack_value
module Pack_store = Pack_store

(* extra cruft from here *)

let close_any_read_logger () = match !Pack_store_IO.Private.read_logger with
  | None -> ()
  | Some oc -> (close_out_noerr oc; Pack_store_IO.Private.read_logger:=None)

module Pack_store_IO = Pack_store_IO
