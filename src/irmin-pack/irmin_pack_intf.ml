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
module type Maker_persistent = S.Maker_persistent

module type Sigs = sig
  module Conf = Conf
  module Indexing_strategy = Indexing_strategy
  module Inode = Inode
  module Pack_key = Pack_key
  module Pack_value = Pack_value
  module Pack_store_intf = Pack_store_intf
  (* module Pack_store = Pack_store *)

  val config :
    ?fresh:bool ->
    ?readonly:bool ->
    ?lru_size:int ->
    ?index_log_size:int ->
    ?merge_throttle:Conf.merge_throttle ->
    ?freeze_throttle:Conf.freeze_throttle ->
    ?indexing_strategy:Indexing_strategy.t ->
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
        does not block but indefinitely expands the in-memory cache.
      @param indexing_strategy
        The {{!Indexing_strategy} indexing strategy} of the backend
        store. Defaults to {!Indexing_strategy.default}. *)

  exception RO_not_allowed

  module type S = S
  module type Specifics = Specifics
  module type Maker = Maker
  module type Maker_persistent = Maker_persistent

  module Stats = Stats
  module Layout = Layout
  module Indexable = Indexable
  module Atomic_write = Atomic_write
  module IO = IO
  module Version = Version
end
