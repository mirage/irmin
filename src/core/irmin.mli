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

(** API entry point *)

(** {2 Stores} *)

module type S = sig

  (** At high-level,Irminsule exposes the same interface as a
      low-level mutable store, but you gain the commit, rollback and
      notification mechanisms. *)

  type t
  (** Type of states. *)

  include IrminStore.M

  val list: key -> key list Lwt.t
  (** The store is supposed to be hierarchical. This function lists
      the children keys of a given key. Useful to walk in the tree
      structure of the database. *)

  val snapshot: unit -> t Lwt.t
  (** Commit the current store state. *)

  val revert: t -> unit Lwt.t
  (** Revert to a previous state. *)

  val watch: key -> (key * t option) Lwt_stream.t
  (** Event stream attached for a given path. *)

end

module type STORE = sig

  (** {2 Main signature for Irminsule stores} *)

  type key
  (** Type of keys. *)

  type value
  (** Type of values. *)

  type tag
  (** Type of tags. *)

  module Value: IrminValue.STORE
    with type key = key
     and type t = value
  (** Persist raw values. *)

  module Tree: IrminTree.STORE
    with type key = key
     and type value = value
  (** Persisit trees. *)

  module Revision: IrminRevision.STORE
    with type key = key
     and type tree = Tree.t
  (** Persist revisions. *)

  module Tag: IrminTag.STORE
    with type t = tag
     and type key = key
  (** Persists tags. *)

  (** {2 Mutable store interface} *)

  module type S = S with type t := key
                     and type key := Tree.path
                     and type value := value

  include S
  (** The main store, associated to the HEAD tag. *)

  val create: Tag.t -> (module S)
  (** Get the mutable store associated to the given tag. If the tag
      does not exist, create a fresh store. *)

end

module Make
  (Key: IrminKey.S)
  (Value: IrminValue.S)
  (Tag: IrminTag.S)
  (SValue: IrminStore.IRAW with type key = Key.t)
  (STree: IrminStore.IRAW with type key = Key.t)
  (SRevision: IrminStore.IRAW with type key = Key.t)
  (STag: IrminStore.MRAW with type value = Key.t)
  : STORE with type key = Key.t
           and type value = Value.t
           and type tag = Tag.t

module type SIMPLE = sig

  (** Simple instantiations for key, value and tag types. Use only the
      master branch. *)

  module Key: module type of IrminKey.SHA1
  (** SHA1 keys. *)

  module Value: module type of IrminValue.Simple
  (** String values. *)

  module Tag: module type of IrminTag.Simple
  (** String tags. *)

  module Store: STORE
    with type key = Key.t
     and type value = Value.t
     and type tag = Tag.t
  (** Signature for simple stores. *)

end

module Simple
    (I: IrminStore.IRAW with type key = IrminKey.SHA1.t)
    (M: IrminStore.MRAW with type value = IrminKey.SHA1.t)
  : SIMPLE
(** Create a simple store. Use only one mutable store for value,
    tree and revisions and a mutable store for the tags. *)
