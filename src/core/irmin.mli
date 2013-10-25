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

module type S = sig

  (** {2 Main signature for Irminsule stores} *)

  type key
  (** Type of keys. *)

  type value
  (** Type of values. *)

  type tag
  (** Type of tags. *)

  module Key: IrminKey.S
    with type t = key
  (** Type of keys. *)

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
  }
  (** Database state. *)

  (** {2 Mutable store interface} *)

  include IrminStore.S with type t := t
                        and type key := Tree.path
                        and type value := value
                        and type revision := key

end

module Make
  (Key: IrminKey.S)
  (Value: IrminValue.S)
  (Tag: IrminTag.S)
  (SValue: IrminStore.A_RAW with type key = Key.t)
  (STree: IrminStore.A_RAW with type key = Key.t)
  (SRevision: IrminStore.A_RAW with type key = Key.t)
  (STag: IrminStore.M_RAW with type value = Key.t)
  : S with type key = Key.t
       and type value = Value.t
       and type tag = Tag.t


module Simple
    (A: IrminStore.A_RAW with type key = IrminKey.SHA1.t)
    (M: IrminStore.M_RAW with type value = IrminKey.SHA1.t)
  : S with type key = IrminKey.SHA1.t
       and type value = IrminValue.Simple.t
       and type tag = IrminTag.Simple.t
(** Create a simple store. Use only one mutable store for value,
    tree and revisions and a mutable store for the tags. *)
