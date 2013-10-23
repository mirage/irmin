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

(** {2 Base types} *)

module type S = sig

  (** Signature for base types. *)

  module Key: IrminKey.S
  (** Type of keys. *)

  module Value: IrminValue.S
  (** Type of values. *)

  module Tag: IrminTag.S
  (** Type of tags. *)

end

module Simple: sig

  (** Simple implementation of base types. *)

  module Key: module type of IrminKey.SHA1
  (** SHA1 keys. *)

  module Value: module type of IrminValue.Simple
  (** String values. *)

  module Tag: module type of IrminTag.Simple
  (** String tags. *)

end

(** {2 Stores} *)

module type STORE = sig

  (** Main signature for Irminsule applications. *)

  module Base: S
  (** Base elements. *)

  (** {2 Main signature for Irminsule stores} *)

  module Value: IrminValue.STORE
    with type key = Base.Key.t
     and type t = Base.Value.t
  (** Persist raw values. *)

  module Tree: IrminTree.STORE
    with type key = Base.Key.t
     and type t = Value.t
  (** Persisit trees. *)

  module Revision: IrminRevision.STORE
    with type key = Base.Key.t
     and type t = Tree.t
  (** Persist revisions. *)

  module Tag_store: IrminTag.STORE
    with type key = Base.Key.t
     and type tree = Tree.t
     and type revision = Revision.t
     and type t = Base.Tag.t
  (** Persists tags. *)

  val name: string
  (** Store name. *)

end

module Make (B: S): STORE with module Base = B

val client: string -> (module STORE)
val memory: unit -> (module STORE)
val fs: string -> (module STORE)
