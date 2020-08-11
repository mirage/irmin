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

module type ELT = sig
  include Irmin.Type.S

  type hash

  val hash : t -> hash

  val magic : t -> char

  val encode_bin :
    dict:(string -> int option) ->
    offset:(hash -> int64 option) ->
    t ->
    hash ->
    (string -> unit) ->
    unit

  val decode_bin :
    dict:(int -> string option) -> hash:(int64 -> hash) -> string -> int -> t
end

module type S = S.CONTENT_ADDRESSABLE_STORE

module type MAKER = sig
  type key

  type index

  (** Save multiple kind of values in the same pack file. Values will be
      distinguished using [V.magic], so they have to all be different. *)
  module Make (V : ELT with type hash := key) :
    S.CONTENT_ADDRESSABLE_STORE
      with type key = key
       and type value = V.t
       and type index = index
end

module File (Index : Pack_index.S) (K : Irmin.Hash.S with type t = Index.key) :
  MAKER with type key = K.t and type index = Index.t
