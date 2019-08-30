(*
 * Copyright (c) 2013-2017 Thomas Gazagnaire <thomas@gazagnaire.org>
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

(** Branch-consistent stores: read-write store with support fork/merge
    operations. *)

module Make (P : S.PRIVATE) :
  S.STORE
    with type key = P.Node.Path.t
     and type contents = P.Contents.value
     and type branch = P.Branch.key
     and type hash = P.Hash.t
     and type slice = P.Slice.t
     and type step = P.Node.Path.step
     and type metadata = P.Node.Val.metadata
     and module Key = P.Node.Path
     and type repo = P.Repo.t
     and module Private = P

module Content_addressable
    (X : S.APPEND_ONLY_STORE_MAKER)
    (K : S.HASH)
    (V : Type.S) : sig
  include
    S.CONTENT_ADDRESSABLE_STORE
      with type 'a t = 'a X(K)(V).t
       and type key = K.t
       and type value = V.t

  val batch : [ `Read ] t -> ([ `Read | `Write ] t -> 'a Lwt.t) -> 'a Lwt.t

  val v : Conf.t -> [ `Read ] t Lwt.t

  val close : 'a t -> unit Lwt.t
end
