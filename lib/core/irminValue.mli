(*
 * Copyright (c) 2013-2014 Thomas Gazagnaire <thomas@gazagnaire.org>
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

(** Structured values: contents, node or commits. *)

type ('key, 'contents) t =
  | Contents of 'contents
  | Node of 'key IrminNode.t
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

  type contents
  (** Contents. *)

  include IrminContents.S with type t = (key, contents) t
  (** Base functions over structured values. *)

end

module S (K: IrminKey.S) (C: IrminContents.S): S with type key = K.t and type contents = C.t

module String: S with type key = IrminKey.SHA1.t and type contents = IrminContents.String.t
(** String contents, with SHA1 keys. *)

module JSON: S with type key = IrminKey.SHA1.t and type contents = IrminContents.JSON.t
(** JSON contents, with SHA1 keys. *)

module type STORE = sig

  (** Value are stored in an append-only database. *)

  type key
  (** Key objects. *)

  type contents
  (** Contents values. *)

  include IrminStore.AO with type key := key
                         and type value = (key, contents) t

  module Contents: IrminContents.STORE
    with type key = key
     and type value = contents

  module Node: IrminNode.STORE
    with type key = key
     and type contents = contents

  module Commit: IrminCommit.STORE
    with type key = key

  val contents: t -> Contents.t
  (** The handler for the contents database. *)

  val node: t -> Node.t
  (** The handler for the node database. *)

  val commit: t -> Commit.t
  (** The handler for the commit database. *)

  module Key: IrminKey.S with type t = key
  (** Base functions over keys. *)

  module Value: S with type key = key and type contents = contents
  (** Base functions over values. *)

end

module Make
  (K: IrminKey.S)
  (C: IrminContents.S)
  (S: IrminStore.AO with type key = K.t and type value = (K.t, C.t) t)
  : STORE with type key = K.t
           and type contents = C.t
(** Create a store for structured values. *)

module Mux
  (K: IrminKey.S)
  (C: IrminContents.S)
  (Contents: IrminStore.AO with type key = K.t and type value = C.t)
  (Node    : IrminStore.AO with type key = K.t and type value = K.t IrminNode.t)
  (Commit  : IrminStore.AO with type key = K.t and type value = K.t IrminCommit.t)
  : STORE with type key = K.t
           and type contents = C.t
(** Combine multiple stores to create a global store for structured
    values. *)
