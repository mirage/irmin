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

(** Manage the database history. *)

module type S = sig

  (** The database history is a partial-order of revisions. *)

  include IrminValue.S
  (** Revisions are values. *)

  type key
  (** Type of keys.*)

  type tree
  (** Type of trees. *)

  val create: ?tree:key -> key list -> t
  (** Create a new revision. *)

  val tree: t -> key option
  (** Get the revision tree. *)

  val parents: t -> key list
  (** Get the immmediate precessors. *)

end

module Make (K: IrminKey.S): S with type key = K.t
(** Create an implementation of revisions using [K] as keys.. *)

module Simple: S with type key = IrminKey.SHA1.t
(** Simple implementation where keys are the SHA1 of values. *)

(** {2 Store} *)

module type STORE = sig

  (** The *key store* is graph of keys where you can only add new
      vertex. *)

  type t
  (** Database handler. *)

  type key
  (** Type of keys. *)

  type revision
  (** Type of revisions. *)

  type tree
  (** Type of trees. *)

  val create: t -> ?tree:tree -> revision list -> revision
  (** Create a new revision. *)

  val key: revision -> key
  (** Compute a deterministic key from a revision. *)

  val write: t -> revision -> key -> unit Lwt.t

  val read: t -> key -> revision option Lwt.t

end
