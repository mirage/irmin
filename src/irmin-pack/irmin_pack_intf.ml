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

module type S = S.S
module type Specifics = S.Specifics
module type Maker = S.Maker
module type Maker_persistent = functor (_ : Conf.S) -> S.Maker_persistent

module type Sigs = sig
  module Dict = Pack_dict
  module Index = Pack_index
  module Conf = Conf
  module Inode = Inode
  module Pack_key = Pack_key
  module Pack_value = Pack_value
  module Pack_store = Pack_store

  val config :
    ?fresh:bool ->
    ?readonly:bool ->
    ?lru_size:int ->
    ?index_log_size:int ->
    ?merge_throttle:Conf.merge_throttle ->
    ?freeze_throttle:Conf.freeze_throttle ->
    ?indexing_strategy:Pack_store.Indexing_strategy.t ->
    string ->
    Irmin.config
  (** Configuration options for stores.

      @param fresh whether an existing store should be overwritten.
      @param read_only whether read-only mode is enabled for this store.
      @param lru_size the maximum number of bindings in the lru cache.
      @param index_log_size the maximum number of bindings in the index cache.
      @param index_throttle
        the strategy to use when the index cache is full and an async
        [Index.merge] in already in progress. [Block_writes] (the default)
        blocks any new writes until the merge is completed. [Overcommit_memory]
        does not block but indefinitely expands the in-memory cache. *)

  exception RO_not_allowed

  module KV (_ : Conf.S) :
    Irmin.Generic_key.KV_maker
      with type metadata = unit
       and type ('h, 'v) contents_key = 'h Pack_key.t
       and type 'h node_key = 'h Pack_key.t
       and type 'h commit_key = 'h Pack_key.t

  module type S = S
  module type Specifics = Specifics
  module type Maker = Maker
  module type Maker_persistent = Maker_persistent

  module Maker : Maker_persistent
  module Stats = Stats
  module Layout = Layout
  module Checks = Checks
  module Indexable = Indexable
  module Atomic_write = Atomic_write
  module IO = IO
  module Utils = Utils
end
