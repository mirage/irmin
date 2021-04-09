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

module Pack = Pack
module Dict = Pack_dict
module Index = Pack_index
module Config = Config
module Inode = Inode
module Version = Version

val config :
  ?fresh:bool ->
  ?readonly:bool ->
  ?lru_size:int ->
  ?index_log_size:int ->
  ?merge_throttle:Config.merge_throttle ->
  ?freeze_throttle:Config.freeze_throttle ->
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

module KV (_ : Version.S) (Config : Config.S) :
  Irmin.KV_maker with type metadata = unit

module Maker_ext
    (_ : Version.S)
    (Config : Config.S)
    (N : Irmin.Private.Node.S)
    (CT : Irmin.Private.Commit.S with type hash = N.hash) : sig
  module Make
      (Metadata : Irmin.Metadata.S with type t = N.metadata)
      (Contents : Irmin.Contents.S)
      (Path : Irmin.Path.S with type step = N.step)
      (Branch : Irmin.Branch.S)
      (Hash : Irmin.Hash.S with type t = N.hash) : sig
    include
      Irmin.S
        with type key = Path.t
         and type contents = Contents.t
         and type branch = Branch.t
         and type hash = Hash.t
         and type step = Path.step
         and type metadata = Metadata.t
         and type Key.step = Path.step
         and type Private.Remote.endpoint = unit

    include Store.S with type repo := repo

    val reconstruct_index : ?output:string -> Irmin.config -> unit

    val integrity_check_inodes :
      ?heads:commit list ->
      repo ->
      ([> `Msg of string ], [> `Msg of string ]) result Lwt.t
  end
end

module Atomic_write (_ : Version.S) (K : Irmin.Type.S) (V : Irmin.Hash.S) : sig
  include Irmin.Atomic_write.S with type key = K.t and type value = V.t

  val v : ?fresh:bool -> ?readonly:bool -> string -> t Lwt.t
end

module Stats = Stats
module Layout = Layout
module Checks = Checks
module Store = Store

module Private : sig
  module Closeable = Closeable
  module Inode = Inode
  module IO = IO
  module Pack_index = Pack_index
  module Sigs = S
  module Utils = Utils
end
