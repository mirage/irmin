(*
 * Copyright (c) 2026 Tarides
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

(** Chunk meta-backend for [irmin-lwt].

    Wraps a Lwt-typed [Append_only.Maker] into a Lwt-typed
    [Content_addressable.Maker] that stores values cut into fixed-size chunks.
    Internally bridges the user's Lwt Maker to its Eio counterpart, runs it
    through [Irmin_chunk.Content_addressable], and bridges the resulting Eio
    [Content_addressable.Maker] back to Lwt. *)

module Conf = Irmin_chunk.Conf

val config :
  ?size:int ->
  ?min_size:int ->
  ?chunking:[ `Max | `Best_fit ] ->
  Irmin_lwt.config ->
  Irmin_lwt.config
(** [config ?size ?min_size ?chunking c] extends the configuration [c] with the
    chunking parameters. See {!Irmin_chunk.config}. *)

(** [Content_addressable (S)] is a Lwt-typed content-addressable store that
    stores values cut into chunks into the underlying Lwt-typed append-only
    store [S]. *)
module Content_addressable (S : Irmin_lwt.Append_only.Maker) :
  Irmin_lwt.Content_addressable.Maker
