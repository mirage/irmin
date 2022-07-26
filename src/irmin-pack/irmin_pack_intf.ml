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
  module type Specifics = Specifics
  module type Maker = Maker
  module type Maker_persistent = Maker_persistent

  module Stats = Stats
  module Layout = Layout
  module Indexable = Indexable
  module Atomic_write = Atomic_write
  module Version = Version
  module S = S
end
