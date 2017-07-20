 (*
 * Copyright (c) 2013-2015 Thomas Gazagnaire <thomas@gazagnaire.org>
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

(**
    This package provides an Irmin backend to cut raw contents into blocks
    of the same size, while preserving the keys used in the store. It can
    be used to optimize space usage when dealing with large files or as a
    an intermediate layer for a raw block device backend.

    # Install

    Use opam:

    ```shell
    opam install irmin-chunk
    ```

    # Use

    ```ocaml
    (* Build an Irmin store, where blobs are cut into chunks of same size *)
    module AO = Irmin_chunk.AO_stable(Irmin_mem.Link)(Irmin_mem.AO)
    module Store = Irmin.Make(AO)(Irmin_mem.RW)
    ```
*)

(** Managing Chunks.

    This module exposes functors to store raw contents into
    append-only stores as chunks of same size. It exposes the
    {{!AO}AO} functor which split the raw contents into [Data] blocks,
    addressed by [Node] blocks. That's the usual rope-like
    representation of strings, but chunk trees are always build as
    perfectly well-balanced and blocks are addressed by their hash (or
    by the stable keys returned by the underlying store).

    A chunk has the following structure:

    {v
     --------------------------
     | uint8_t type            |
     ---------------------------
     | uint16_t length         |
     ---------------------------
     | byte data[length]       |
     ---------------------------
v}

    [type] is either [Data] (0) or [Node] (1). If the chunk contains
    data, [length] is the payload length. Otherwise it is the number
    of children that the node has.

    It also exposes {{!AO_stable}AO_stable} which -- as {{!AO}AO} does
    -- stores raw contents into chunks of same size. But it also
    preserves the nice properpty that values are addressed by their
    hash. instead of by the hash of the root chunk node as it is the
    case for {{!AO}AO}.
*)

val chunk_size: int Irmin.Private.Conf.key
(** [chunk_size] is the configuration key to configure chunks'
    size. By default, it is set to 4666, so that payload and metadata
    can be stored in a 4K block. *)

val config:
  ?config:Irmin.config -> ?size:int -> ?min_size:int -> unit -> Irmin.config
(** [config ?config ?size ?min_size ()] is the configuration value
    extending the optional [config] with bindings associating
    {{!chunk_size}chunk_size} to [size].

    Fail with [Invalid_argument] if [size] is smaller than [min_size].
    [min_size] is, by default, set to 4000 (to avoid hash colision on
    smaller size) but can be tweaked for testing purposes. {i Notes:}
    the smaller [size] is, the bigger the risk of hash collisions, so
    use reasonable values. *)

module AO (S: Irmin.AO_MAKER): Irmin.AO_MAKER
(** [AO(X)] is an append-only store which store values cut into chunks
    into the underlying store [X].

    The keys returns by [add] are the hash of the chunked values: it could
    either be a full block if the value is small, or a tree node if
    the values need to be cut into chunks.

    In both case, the return hash will be different from the hash of
    the value. This discrepency can be fixed using
    {{!AO_stable}AO_stable}, at the cost of adding an indirection on
    reads. *)

module AO_stable (L: Irmin.LINK_MAKER) (S: Irmin.AO_MAKER):
  Irmin.AO_MAKER
(** [AO_stable(L)(X)] is similar to [AO(X)] but is ensures that the
    return keys are similar as if they were stored directly in [X], so
    that the fact that blobs are cut into chunks is an implementation
    detail. *)
