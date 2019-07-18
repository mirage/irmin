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

module type CONFIG = sig
  val entries : int

  val stable_hash : int
end

module type S = sig
  include Irmin.CONTENT_ADDRESSABLE_STORE

  val v :
    ?fresh:bool ->
    ?shared:bool ->
    ?readonly:bool ->
    ?lru_size:int ->
    string ->
    [ `Read ] t Lwt.t

  val batch : [ `Read ] t -> ([ `Read | `Write ] t -> 'a Lwt.t) -> 'a Lwt.t

  module Key : Irmin.Hash.S with type t = key

  module Val :
    Irmin.Private.Node.S
    with type t = value
     and type hash = key
     and type t = value
end

module Make
    (Conf : CONFIG)
    (P : Pack.MAKER)
    (H : Irmin.Hash.S with type t = P.key)
    (Node : Irmin.Private.Node.S with type hash = H.t) :
  S
  with type key = H.t
   and type Val.metadata = Node.metadata
   and type Val.step = Node.step
