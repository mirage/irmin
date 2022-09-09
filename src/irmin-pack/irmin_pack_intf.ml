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
open! Import

exception RO_not_allowed

module type Checkable = sig
  type 'a t
  type hash

  val integrity_check :
    offset:int63 ->
    length:int ->
    hash ->
    _ t ->
    (unit, [ `Wrong_hash | `Absent_value ]) result
end

module type S = Irmin.Generic_key.S

module S_is_a_store (X : S) : Irmin.Generic_key.S = X

module type Maker = sig
  type endpoint = unit

  include Irmin.Key.Store_spec.S

  module Make (Schema : Irmin.Schema.Extended) :
    S
    (* We can't have `with module Schema = Schema` here, since the Schema
       on the RHS contains more information than the one on the LHS. We _want_
       to do something like `with module Schema = (Schema : Irmin.Schema.S)`,
       but this isn't supported.

       TODO: extract these extensions as a separate functor argument instead. *)
      with type Schema.Hash.t = Schema.Hash.t
       and type Schema.Branch.t = Schema.Branch.t
       and type Schema.Metadata.t = Schema.Metadata.t
       and type Schema.Path.t = Schema.Path.t
       and type Schema.Path.step = Schema.Path.step
       and type Schema.Contents.t = Schema.Contents.t
       and type Schema.Info.t = Schema.Info.t
       and type contents_key = (Schema.Hash.t, Schema.Contents.t) contents_key
       and type node_key = Schema.Hash.t node_key
       and type commit_key = Schema.Hash.t commit_key
       and type Backend.Remote.endpoint = endpoint
end

module type Sigs = sig
  module Conf = Conf
  module Indexing_strategy = Indexing_strategy
  module Inode = Inode
  module Pack_key = Pack_key
  module Pack_value = Pack_value

  val config :
    ?fresh:bool ->
    ?readonly:bool ->
    ?lru_size:int ->
    ?index_log_size:int ->
    ?merge_throttle:Conf.merge_throttle ->
    ?indexing_strategy:Indexing_strategy.t ->
    ?use_fsync:bool ->
    ?dict_auto_flush_threshold:int ->
    ?suffix_auto_flush_threshold:int ->
    ?no_migrate:bool ->
    string ->
    Irmin.config
  (** Configuration options for stores. See {!Irmin_pack.Conf} for more details. *)

  exception RO_not_allowed

  module type S = S
  module type Maker = Maker
  module type Checkable = Checkable

  module Stats = Stats
  module Layout = Layout
  module Indexable = Indexable
  module Atomic_write = Atomic_write
  module Version = Version
end
