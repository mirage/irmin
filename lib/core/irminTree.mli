(*
 * Copyright (c) 2013 Louis Gesbert     <louis.gesbert@ocamlpro.com>
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

(** Tree-like structures of values. *)

type 'key t = {
  blob    : 'key option;
  children: (string * 'key) list;
} with bin_io, compare, sexp
(** Type of trees .*)

val empty: 'key t
(** The empty tree. *)

module type S = sig

  (** Signature for trees. *)

  type key
  (** Keys. *)

  include IrminBlob.S with type key := key and type t = key t

end

module S (K: IrminKey.S): S with type key = K.t
(** Base functions for trees. *)

module SHA1: S with type key = IrminKey.SHA1.t
(** Simple tree implementation, where keys are SHA1s. *)

module type STORE = sig

  (** Tree stores. *)

  type key
  (** Type of keys. *)

  type blob
  (** Type of blobs. *)

  type value = key t
  (** Type of values. *)

  include IrminStore.AO with type key := key
                         and type value := value
  (** Tree stores are append-only. *)

  val tree: t -> ?value:blob -> (string * value) list -> key Lwt.t
  (** Create a new node. *)

  val blob: t -> value -> blob Lwt.t option
  (** Return the contents. *)

  val children: t -> value -> (string * value Lwt.t) list
  (** Return the child nodes. *)

  val sub: t -> value -> IrminPath.t -> value option Lwt.t
  (** Find a subvalue. *)

  val sub_exn: t -> value -> IrminPath.t -> value Lwt.t
  (** Find a subvalue. Raise [Not_found] if it does not exist. *)

  val update: t -> value -> IrminPath.t -> blob -> value Lwt.t
  (** Add a value by recusively saving subvalues and subvalues into the
      corresponding stores. *)

  val find: t -> value -> IrminPath.t -> blob option Lwt.t
  (** Find a value. *)

  val find_exn: t -> value -> IrminPath.t -> blob Lwt.t
  (** Find a value. Raise [Not_found] is [path] is not defined. *)

  val remove: t -> value -> IrminPath.t -> value Lwt.t
  (** Remove a value. *)

  val valid: t -> value -> IrminPath.t -> bool Lwt.t
  (** Is a path valid. *)

  module Key: IrminKey.S with type t = key
  (** Base functions for keys. *)

  module Value: S with type key = key
  (** Base functions for values. *)

end

module Make
    (K: IrminKey.S)
    (B: IrminBlob.S with type key = K.t)
    (Blob: IrminStore.AO with type key = K.t and type value = B.t)
    (Tree: IrminStore.AO with type key = K.t and type value = K.t t)
  : STORE with type t = Blob.t * Tree.t
           and type key = K.t
           and type blob = B.t
(** Create a tree store from an append-only database. *)
