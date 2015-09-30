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

(** Managing Chunks.

     All functions for manipulate chunks representation, a chunk is
     represented as above :

      --------------------------
     | uint8_t type            |
     ---------------------------
     | uint16_t length         |
     ---------------------------
     | byte data[length]       |
     ---------------------------

     Where [type] define if the chunk contains raw data or is a node
     and [length] is the [data] payload's length. *)

val chunk_size: int Irmin.Private.Conf.key
(** The key to configure the size of chunks. By default, it is set to
    4666 (to let some space for metadata). *)

val config:
  ?config:Irmin.config -> ?size:int -> ?min_size:int -> unit -> Irmin.config
(** [config ?config ?size ?min_size ()] is the configuration value
    extending the optional [config] with bindings associating
    {!chunk_size} to [size].

    Fail with [Invalid_argument] if [size] is smaller than [min_size].
    [min_size] is, by default, set to 4000 (to avoid hash colision on
    smaller size) but can be tweaked for testing purposes. {i Notes:}
    the smaller [size] is, the bigger the risk of hash collisions, so
    use reasonable values. *)

module AO (S: Irmin.AO_MAKER_RAW): Irmin.AO_MAKER_RAW
(** [AO(X)] is an append-only store which store values cut into chunks
    into the underlying store [X]. The keys returns by [add] are the
    hash of the chunked values: it could either be a full block if the
    value is small, or a tree node if the values need to be cut into
    chunks.

    In both case, the return hash will be different from the hash of
    the value. This discrepency can be fixed using the an
    {{!Irmin.LINK}immutable link store}. *)

module AO_stable (L: Irmin.LINK_MAKER) (S: Irmin.AO_MAKER_RAW):
  Irmin.AO_MAKER_RAW
(** [AO_stable(L)(X)] is similar to [AO(X)] but is ensures that the
    return keys are similar as if they were stored directly in [X], so
    that the fact that blobs are cut into chunks is an implementation
    detail. *)
