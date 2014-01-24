(*
 * Copyright (c) 2013 Thomas Gazagnaire <thomas@gazagnaire.org>
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

(** Structured values: blob, tree or commits. *)

type ('key, 'blob) t =
  | Blob of 'blob
  | Tree of 'key IrminTree.t
  | Commit of 'key IrminCommit.t
with bin_io, compare, sexp
(** The different kinds of values which can be stored in the
    database. *)

val of_json: (Ezjsonm.t -> 'a) -> (Ezjsonm.t -> 'b) -> Ezjsonm.t -> ('a, 'b) t
val to_json: ('a -> Ezjsonm.t) -> ('b -> Ezjsonm.t) -> ('a, 'b) t -> Ezjsonm.t

module type S = sig

  (** Signature for structured values. *)

  type key
  (** Keys. *)

  type blob
  (** Blobs. *)

  include IrminBlob.S with type t = (key, blob) t
  (** Base functions over structured values. *)

end

module S (K: IrminKey.S) (B: IrminBlob.S): S with type key = K.t and type blob = B.t


module String: S with type key = IrminKey.SHA1.t and type blob = IrminBlob.String.t
(** String blobs, with SHA1 keys. *)

module JSON: S with type key = IrminKey.SHA1.t and type blob = IrminBlob.JSON.t
(** JSON blobs, with SHA1 keys. *)

module type STORE = sig

  (** Value are stored in an append-only database. *)

  type key
  (** Key objects. *)

  type blob
  (** Blob values. *)

  include IrminStore.AO with type key := key
                         and type value = (key, blob) t

  module Blob: IrminBlob.STORE
    with type key = key
     and type value = blob

  module Tree: IrminTree.STORE
    with type key = key
     and type blob = blob

  module Commit: IrminCommit.STORE
    with type key = key

  val blob: t -> Blob.t
  (** The handler for the blob database. *)

  val tree: t -> Tree.t
  (** The handler for the tree database. *)

  val commit: t -> Commit.t
  (** The handler for the commit database. *)

  module Key: IrminKey.S with type t = key
  (** Base functions over keys. *)

  module Value: S with type key = key and type blob = blob
  (** Base functions over values. *)

end

module Make
  (K: IrminKey.S)
  (B: IrminBlob.S)
  (S: IrminStore.AO with type key = K.t and type value = (K.t, B.t) t)
  : STORE with type t = S.t
           and type key = K.t
           and type blob = B.t
(** Create a store for structured values. *)

module Mux
  (K: IrminKey.S)
  (B: IrminBlob.S)
  (Blob: IrminStore.AO with type key = K.t and type value = B.t)
  (Tree: IrminStore.AO with type key = K.t and type value = K.t IrminTree.t)
  (Commit: IrminStore.AO with type key = K.t and type value = K.t IrminCommit.t)
  : STORE with type key = K.t
           and type blob = B.t
(** Combine multiple stores to create a global store for structured
    values. *)
