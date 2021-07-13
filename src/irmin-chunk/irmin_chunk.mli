(*
 * Copyright (c) 2013-2021 Thomas Gazagnaire <thomas@gazagnaire.org>
 * Copyright (c) 2015 Mounir Nasr Allah <mounir@nasrallah.co>
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

(** This package provides an Irmin backend to cut raw contents into blocks of
    the same size, while preserving the keys used in the store. It can be used
    to optimize space usage when dealing with large files or as an intermediate
    layer for a raw block device backend. *)

(** {1 Managing Chunks}

    This module exposes functors to store raw contents into append-only stores
    as chunks of same size. It exposes the {{!AO} AO} functor which split the
    raw contents into [Data] blocks, addressed by [Node] blocks. That's the
    usual rope-like representation of strings, but chunk trees are always built
    as perfectly well-balanced and blocks are addressed by their hash (or by the
    stable keys returned by the underlying store).

    A chunk has the following structure:

    {v
     --------------------------      --------------------------
     | uint8_t type            |     | uint8_t type            |
     ---------------------------     ---------------------------
     | uint16_t                |     | uint64_t                |
     ---------------------------     ---------------------------
     | key children[length]    |     | byte data[length]       |
     ---------------------------     ---------------------------
    v}

    [type] is either [Data] (0) or [Index] (1). If the chunk contains data,
    [length] is the payload length. Otherwise it is the number of children that
    the node has.

    It also exposes {{!AO_stable} AO_stable} which -- as {{!AO} AO} does --
    stores raw contents into chunks of same size. But it also preserves the nice
    property that values are addressed by their hash, instead of by the hash of
    the root chunk node as is the case for {{!AO} AO}. *)

module Conf : sig
  include Irmin.Private.Conf.S

  module Key : sig
    val chunk_size : int key
    (** [chunk_size] is the configuration key to configure chunk size. By
        default, it is set to 4666, so that payload and metadata can be stored
        in a 4K block. *)

    val min_size : int key
    val chunking : [ `Best_fit | `Max ] key
  end
end

val config :
  ?config:Irmin.config ->
  ?size:int ->
  ?min_size:int ->
  ?chunking:[ `Max | `Best_fit ] ->
  unit ->
  Irmin.config
(** [config ?config ?size ?min_size ()] is the configuration value extending the
    optional [config] with bindings associating {{!chunk_size} chunk_size} to
    [size].

    If [chunking] is [Best_fit] (the default), the size of new chunks will be of
    maximum [max_size] but could be smaller if they don't need to be chunked. If
    [chunking] is [Max], all the new chunks will be of size [max_size].

    Fail with [Invalid_argument] if [size] is smaller than [min_size].
    [min_size] is, by default, set to 4000 (to avoid hash collisions on smaller
    sizes) but can be tweaked for testing purposes. {i Notes:} the smaller
    [size] is, the bigger the risk of hash collisions, so use reasonable values. *)

(** [Content_addressable(X)] is a content-addressable store which store values
    cut into chunks into the underlying store [X]. *)
module Content_addressable (S : Irmin.Append_only.Maker) :
  Irmin.Content_addressable.Maker
