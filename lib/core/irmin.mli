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

  type value
  (** Value for abstract blobs. It's usually a raw string, but it can
      also be a structured value. One should be able to merge two
      diverging blobs. *)

  module Internal: IrminValue.STORE with type contents = value
  (** Append-only persistent store for internal values. *)

  (** {2 Irminsule store interface} *)

  include IrminStore.S with type key      = string list
                        and type value   := value
                        and type snapshot = Internal.key
                        and type dump     = (Internal.key, value) IrminDump.t

  val output: t -> string -> unit Lwt.t
  (** Create a Graphviz graph representing the store state. Could be
      no-op if the backend does not support that operation (for instance,
      for remote connections). *)

  module Reference: IrminReference.STORE with type value = Internal.key
  (** Read/write store for references. *)

  val internal: t -> Internal.t
  (** Return an handler to the internal store. *)

  val reference: t -> Reference.t
  (** Return an handler to the reference store. *)

  val branch: t -> Reference.key
  (** Return the current branch reference. *)

  module Key: IrminKey.S with type t = key
  (** Base functions over keys. *)

  module Value: IrminContents.S with type t = value
  (** Base functions over values. *)

  module Snapshot: IrminKey.S with type t = snapshot
  (** Base functions over snapshots. *)

  module Dump: IrminDump.S with type key = Internal.key and type contents = value
  (** Base functions over dumps. *)

end

module Make
    (K : IrminKey.S)
    (B : IrminContents.S)
    (R : IrminReference.S)
    (Internal : IrminValue.STORE with type key = K.t and type contents = B.t)
    (Reference: IrminReference.STORE with type key = R.t and type value = K.t)
  : S with type value = B.t
       and module Internal = Internal
       and module Reference = Reference
(** Build a complete iminsule store. *)

module type SHA1 = S
  with type Internal.key = IrminKey.SHA1.t
   and type Reference.key = IrminReference.String.t
(** Signature for stores with SHA1 keys. *)

module type STRING = SHA1 with type value = IrminContents.String.t
(** Signature for stores with SHA1 keys and string values. *)

module String (AO: IrminStore.AO_BINARY) (RW: IrminStore.RW_BINARY): STRING
(** Create a simple string store. Use only one append-only store for values,
    nodes and commits and a mutable store for the tags. *)

module type JSON = SHA1 with type value = IrminContents.JSON.t
(** Signature for SHA1 stores with JSON values. *)

module JSON (AO: IrminStore.AO_BINARY) (RW: IrminStore.RW_BINARY): JSON
(** Create a SJON store. *)

val set_date_hook: (unit -> float) -> unit
(** How to compute the commit dates. By default, increment a counter. *)

val set_origin_hook: (unit -> string) -> unit
(** How to compute the commit origins. By default, return a random number. *)
