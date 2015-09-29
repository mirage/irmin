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
     | uint8_t type             |
     ---------------------------
     | uint16_t chunk_length   |
     ---------------------------
     | byte data[data_length]   |
     ---------------------------

     Where type define if the chunk contain data or indirection, size
     represent the data length to consider, and data field is the
     payload. *)

val chunk_size: int Irmin.Private.Conf.key
(** The key to configure the size of chunks. By default, it is set to
    4666 (to let some space for metadata). *)

val config: ?config:Irmin.config -> ?size:int -> unit -> Irmin.config
(** [config ?config ?size ()] is the configuration value extending the
    optional [config] with a binding associating {!chunk_size} to
    [size]. *)

module AO (S: Irmin.AO_MAKER_RAW): Irmin.AO_MAKER_RAW
(** [AO(X)] is an append-only store which store values cut into chunks
    into the underlying store [X]. The keys returns by [add] are the
    hash of the chunked values: it could either be a full block if the
    value is small, or a tree node if the values need to be cut into
    chunks.

    In both case, the return hash will be different from the hash of
    the value. This discrepency can be fixed using the an
    {{!Irmin.LINK}immutable link store}. *)
