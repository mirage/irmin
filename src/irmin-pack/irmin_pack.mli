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

module Dict = Pack_dict
module Index = Pack_index
module Conf = Conf
module Inode = Inode
module Version = Version

val config :
  ?fresh:bool ->
  ?readonly:bool ->
  ?lru_size:int ->
  ?index_log_size:int ->
  ?merge_throttle:Conf.merge_throttle ->
  ?freeze_throttle:Conf.freeze_throttle ->
  string ->
  Irmin.config
(** Configuration options for stores.

    @param fresh whether an existing store should be overwritten.
    @param read_only whether read-only mode is enabled for this store.
    @param lru_size the maximum number of bindings in the lru cache.
    @param index_log_size the maximum number of bindings in the index cache.
    @param index_throttle the strategy to use when the index cache is full and
    an async [Index.merge] in already in progress. [Block_writes] (the default)
    blocks any new writes until the merge is completed. [Overcommit_memory] does
    not block but indefinitely expands the in-memory cache. *)

exception RO_not_allowed

include Irmin_pack_intf.Sigs
(** @inline *)

module Maker (_ : Version.S) : Maker
module V1 : Maker
module V2 : Maker

module KV (_ : Version.S) (_ : Conf.S) :
  Irmin.KV_maker with type metadata = unit

module type Maker = sig
  include S.Maker
  (** @inline *)
end

module type Specifics = sig
  include S.Specifics
  (** @inline *)
end

module Maker_ext
    (_ : Version.S)
    (Config : Conf.S)
    (N : Irmin.Private.Node.Maker)
    (CT : Irmin.Private.Commit.Maker with type Version.t = Config.version) :
  Maker with type info = CT.Info.t and type version = Config.version

module Stats = Stats
module Layout = Layout
module Checks = Checks

val migrate : Irmin.config -> unit
(** [migrate conf] upgrades the repository with configuration [conf] to use the
    latest storage format.

    {b Note:} performing concurrent store operations during the migration, or
    attempting to use pre-migration instances of the repository after the
    migration is complete, will result in undefined behaviour. *)

module Content_addressable = Content_addressable
module Atomic_write = Atomic_write
module IO = IO
module Utils = Utils
