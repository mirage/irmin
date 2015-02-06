(*
 * Copyright (c) 2013-2015 Thomas Gazagnaire <thomas@gazagnaire.org>
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

module type STORE = sig
  include Ir_ro.STORE
  val iter: t -> (key -> unit Lwt.t) -> unit Lwt.t
  val update: t -> key -> value -> unit Lwt.t
  val remove: t -> key -> unit Lwt.t
  val watch: t -> key -> value option Lwt_stream.t
  val watch_all: t -> (key * value option) Lwt_stream.t
end

module type HIERARCHICAL = sig
  include STORE
  val list: t -> key -> key list Lwt.t
  val remove_rec: t -> key -> unit Lwt.t
end

module type MAKER =
  functor (K: Ir_hum.S) ->
  functor (V: Ir_hash.S) ->
    STORE with type key = K.t and type value = V.t
