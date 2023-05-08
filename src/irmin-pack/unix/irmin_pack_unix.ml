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

(** {1 Store} *)

module type S = Store_intf.S

module Maker = Store.Maker

module KV (Config : Irmin_pack.Conf.S) = struct
  type endpoint = unit
  type hash = Irmin.Schema.default_hash

  include Pack_key.Store_spec
  module Maker = Maker (Config)

  type metadata = Irmin.Metadata.None.t

  module Make (C : Irmin.Contents.S) = Maker.Make (Irmin.Schema.KV (C))
end

(** {1 Key and Values} *)

module Pack_key = Pack_key
module Pack_value = Pack_value

(** {1 Internal} *)

module Stats = Stats
module Index = Pack_index
module Inode = Inode
module Pack_store = Pack_store
module Io_legacy = Io_legacy
module Checks = Checks
module Atomic_write = Atomic_write
module Dict = Dict
module Dispatcher = Dispatcher
module Io = Io
module Async = Async
module Errors = Errors
module Io_errors = Io_errors
module Control_file = Control_file
module Append_only_file = Append_only_file
module Chunked_suffix = Chunked_suffix
module Ranges = Ranges
module Sparse_file = Sparse_file
module File_manager = File_manager
module Lower = Lower
module Utils = Utils
module Lru = Lru
