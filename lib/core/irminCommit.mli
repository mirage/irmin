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

(** Manage the database history. *)

type 'key t = {
  node   : 'key option;
  parents: 'key list;
  origin : IrminOrigin.t;
} with bin_io, compare, sexp
(** Type of concrete revisions. *)

val edges: 'a t -> ('a, 'b) IrminGraph.vertex list
(** The graph edges. *)

val of_json: (Ezjsonm.t -> 'a) -> Ezjsonm.t -> 'a t
val to_json: ('a -> Ezjsonm.t) -> 'a t -> Ezjsonm.t

module type S = sig

  (** Signature for commit objects. *)

  type key
  (** Keys. *)

  include IrminContents.S with type t = key t
  (** Base functions over commit objects. *)

end

module S (K: IrminKey.S): S with type key = K.t

module SHA1: S with type key = IrminKey.SHA1.t
(** Simple implementation where keys are SHA1s. *)

module type STORE = sig

  (** The database history is a partial-order of revisions. *)

  type key
  (** Type of keys. *)

  type value = key t
  (** Type of revisions. *)

  include IrminStore.AO with type key := key
                         and type value := value
  (** Revision stores are append-only. *)

  val commit: t -> IrminOrigin.t ->
    ?node:key IrminNode.t -> parents:value list -> (key * value) Lwt.t
  (** Create a new commit. *)

  val node: t -> value -> key IrminNode.t Lwt.t option
  (** Get the commit node. *)

  val parents: t -> value -> value Lwt.t list
  (** Get the immmediate precessors. *)

  val merge: t -> IrminOrigin.t -> key IrminMerge.t
  (** Lift [S.merge] to the store keys. *)

  module Key: IrminKey.S with type t = key
  (** Base functions over keys. *)

  module Value: S with type key = key
  (** Base functions over values. *)

end

module Make
    (K: IrminKey.S)
    (Node  : IrminNode.STORE with type key = K.t and type value = K.t IrminNode.t)
    (Commit: IrminStore.AO   with type key = K.t and type value = K.t t)
  : STORE with type t = Node.t * Commit.t
           and type key = K.t
(** Create a revision store. *)
