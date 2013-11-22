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

type ('a, 'b) store_dump =
  ('a * ('a, 'b) value_dump) list
(** Type for database dumps. *)

and ('a, 'b) value_dump =
  | Value of 'b
  | Tree of ('a, 'a) IrminTree.node
  | Revision of ('a, 'a) IrminRevision.node
(** Type of value dumps. *)

module type S = sig

  (** {2 Main signature for Irminsule stores} *)

  type key
  (** Type of keys. *)

  type value
  (** Type of values. *)

  type tag
  (** Type of tags. *)

  module Key: IrminKey.BINARY
    with type t = key
  (** Type of binary keys. *)

  module Value: IrminValue.STORE
    with type key = key
     and type value = value
  (** Persistent values. *)

  module Tree: IrminTree.STORE
    with type key = key
     and type value = value
  (** Persisitent trees. *)

  module Revision: IrminRevision.STORE
    with type key = key
     and type tree = Tree.tree
  (** Persistent revisions. *)

  module Tag: IrminTag.STORE
    with type tag = tag
     and type key = key
  (** Persistent tags. *)

  type t = {
    value   : Value.t;
    tree    : Tree.t;
    revision: Revision.t;
    tag     : Tag.t;
    branch  : Tag.tag;
  }
  (** Database state. *)

  (** {2 Mutable store interface} *)

  include IrminStore.S with type t := t
                        and type key := IrminTree.path
                        and type value := value
                        and type revision := key
                        and type dump = (key, value) store_dump

  val tag: Tag.tag -> t Lwt.t
  (** Create a store associated to a given tag (by default, [create]
      uses [Tag.head]. *)

  val output: t -> string -> unit Lwt.t
  (** Create a Graphviz graph representing the store state. *)

  module Dump: IrminBase.S with type t = dump
  (** Basic functions for [dump] values. *)

end

module Make
    (K: IrminKey.BINARY)
    (V: IrminValue.S)
    (T: IrminTag.S)
    (Value   : IrminStore.A_MAKER)
    (Tree    : IrminStore.A_MAKER)
    (Revision: IrminStore.A_MAKER)
    (Tag     : IrminStore.M_MAKER):
  S with type key = K.t
     and type value = V.t
     and type tag = T.t
(** Build an Irminsule store. *)

module Binary
    (K: IrminKey.BINARY)
    (V: IrminValue.S)
    (T: IrminTag.S)
    (Value   : IrminStore.A_BINARY)
    (Tree    : IrminStore.A_BINARY)
    (Revision: IrminStore.A_BINARY)
    (Tag     : IrminStore.M_BINARY):
  S with type key = K.t
     and type value = V.t
     and type tag = T.t
(** Make a binary Irminsule store. *)

module type SIMPLE = S
  with type key = IrminKey.SHA1.t
   and type value = IrminValue.Simple.t
   and type tag = IrminTag.Simple.t
(** Signature for simple stores. *)

module Simple (A: IrminStore.A_BINARY) (M: IrminStore.M_BINARY): SIMPLE
(** Create a simple binary store. Use only one mutable store for
    value, tree and revisions and a mutable store for the tags. *)
