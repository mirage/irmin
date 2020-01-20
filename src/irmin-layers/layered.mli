(*
 * Copyright (c) 2013-2020 Thomas Gazagnaire <thomas@gazagnaire.org>
 *                         Ioana Cristescu <ioana@tarides.com>
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

module type CA = sig
  include Irmin.CONTENT_ADDRESSABLE_STORE

  module Key : Irmin.Hash.TYPED with type t = key and type value = value
end

exception Copy_error of string

module Content_addressable
    (K : Irmin.Hash.S)
    (V : Irmin.Type.S)
    (L : CA with type key = K.t and type value = V.t)
    (U : Irmin.CONTENT_ADDRESSABLE_STORE
           with type key = K.t
            and type value = V.t) : sig
  include
    Irmin.CONTENT_ADDRESSABLE_STORE with type key = K.t and type value = V.t

  val v : 'a U.t -> [ `Read ] L.t -> 'a t

  val project : [ `Read | `Write ] U.t -> 'a t -> [ `Read | `Write ] t

  val layer_id : [ `Read ] t -> key -> int Lwt.t

  val clear_upper : 'a t -> unit Lwt.t

  val already_in_dst :
    dst:[ `Read | `Write ] L.t -> key -> (key -> unit Lwt.t) -> unit Lwt.t

  val copy :
    [ `Read ] t ->
    dst:[ `Read | `Write ] L.t ->
    aux:(value -> unit Lwt.t) ->
    string ->
    key ->
    unit Lwt.t

  val check_and_copy :
    [ `Read ] t ->
    dst:[ `Read | `Write ] L.t ->
    aux:(value -> unit Lwt.t) ->
    string ->
    key ->
    unit Lwt.t
end

module Atomic_write
    (K : Irmin.Type.S)
    (V : Irmin.Hash.S)
    (L : Irmin.ATOMIC_WRITE_STORE with type key = K.t and type value = V.t)
    (U : Irmin.ATOMIC_WRITE_STORE with type key = K.t and type value = V.t) : sig
  include Irmin.ATOMIC_WRITE_STORE with type key = K.t and type value = V.t

  val v : U.t -> L.t -> t

  val clear_upper : t -> unit Lwt.t

  val copy : t -> (value -> bool Lwt.t) -> unit Lwt.t
end
