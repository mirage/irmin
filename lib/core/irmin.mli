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

  module Reference: IrminReference.STORE with type value = Internal.key
  (** Read/write store for references. *)

  include IrminStore.S with type key      = string list
                        and type value   := value
                        and type snapshot = Internal.key
                        and type dump     = (Internal.key, value) IrminDump.t
                        and type branch   = Reference.key

  val output: t -> string -> unit Lwt.t
  (** Create a Graphviz graph representing the store state. Could be
      no-op if the backend does not support that operation (for instance,
      for remote connections). *)

  val internal: t -> Internal.t
  (** Return an handler to the internal store. *)

  val reference: t -> Reference.t
  (** Return an handler to the reference store. *)

  (** {Branches} *)

  val branch: t -> branch -> t Lwt.t
  (** Fork the store, using the giben branch name. *)

  val merge: t -> into:t -> unit Lwt.t
  (** [merge t ~into] merges the branch [t.branch] into
      [into.branch]. Both stores should have the same underlying
      store. Update the commit pointed by [t] to the merge commit of
      the two branches. *)

  module Key: IrminKey.S with type t = key
  (** Base functions over keys. *)

  module Value: IrminContents.S with type t = value
  (** Base functions over values. *)

  module Snapshot: IrminKey.S with type t = snapshot
  (** Base functions over snapshots. *)

  module Dump: IrminDump.S with type key = Internal.key and type contents = value
  (** Base functions over dumps. *)

  module View: IrminView.S with type value := value
  (** Load sub-trees in memory. *)

  val updates: t -> key -> View.t -> unit Lwt.t
  (** Commit a view to the store. *)

  val view: t -> key -> View.t Lwt.t
  (** Build a view from the store. *)

end

type ('key, 'value, 'ref) t =
  (module S with type Internal.key = 'key
             and type value = 'value
             and type Reference.key = 'ref)

module Make
    (K : IrminKey.S)
    (C : IrminContents.S)
    (R : IrminReference.S)
    (Internal : IrminValue.STORE     with type key = K.t and type contents = C.t)
    (Reference: IrminReference.STORE with type key = R.t and type value = K.t)
  : S with type value = C.t
       and module Internal = Internal
       and module Reference = Reference
(** Build a full iminsule store. *)

module Binary
    (K : IrminKey.S)
    (C : IrminContents.S)
    (R : IrminReference.S)
    (AO: IrminStore.AO_BINARY)
    (RW: IrminStore.RW_BINARY)
  : S with type value = C.t
       and type Internal.key = K.t
       and type Reference.key = R.t
(** Create an irminsule store from binary store makers. Use only one
    append-only store for values, nodes and commits and a mutable
    store for the tags. *)

val set_date_hook: (unit -> float) -> unit
(** How to compute the commit dates. By default, increment a counter. *)

val set_origin_hook: (unit -> string) -> unit
(** How to compute the commit origins. By default, return a random number. *)
