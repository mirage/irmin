(*
 * Copyright (c) 2013-2019 Thomas Gazagnaire <thomas@gazagnaire.org>
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

module type S = sig
  include Irmin.Type.S

  type hash

  val hash : t -> hash

  val magic : char

  val encode_bin :
    dict:(string -> int) ->
    offset:(hash -> int64 option) ->
    t ->
    hash ->
    (string -> unit) ->
    unit

  val decode_bin :
    dict:(int -> string option) -> hash:(int64 -> hash) -> string -> int -> t
end

module File (K : Irmin.Hash.S) : sig
  module Make (V : S with type hash = K.t) : sig
    include
      Irmin.CONTENT_ADDRESSABLE_STORE with type key = K.t and type value = V.t

    val v :
      ?fresh:bool ->
      ?shared:bool ->
      ?readonly:bool ->
      ?lru_size:int ->
      string ->
      [ `Read ] t Lwt.t

    val batch : [ `Read ] t -> ([ `Read | `Write ] t -> 'a Lwt.t) -> 'a Lwt.t

    val append : 'a t -> K.t -> V.t -> unit Lwt.t

    val unsafe_append : 'a t -> K.t -> V.t -> unit

    val unsafe_find : 'a t -> K.t -> V.t option

    val sync : 'a t -> unit
  end
end

type stats = {
  pack_cache_misses : float;
  offset_ratio : float;
  offset_significance : int
}

val reset_stats : unit -> unit

val stats : unit -> stats
