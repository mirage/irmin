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

(** Datamodel for Irminsule backends *)

(** {2 Base types} *)

(** Buffered IOs are mutable cstruct buffers. *)
type bufIO = {
  mutable buffer: Cstruct.t;
}

(** It's quite anoying to have to define that again ... *)
module type SET = sig
  include Set.S
  val of_list: elt list -> t
  val to_list: t -> elt list
  val pretty: t -> string
end

(** Base types *)
module type BASE = sig

  (** Abstract type *)
  type t

  (** Compare two elements. *)
  val compare: t -> t -> int

  (** Are two elements equal ? *)
  val equal: t -> t -> bool

  (** Pretty-printing *)
  val pretty: t -> string

  (** Convert from JSON *)
  val of_json: IrminJSON.t -> t

  (** Convert to IrminJSON *)
  val to_json: t -> IrminJSON.t

  (** Size of serialized value (to pre-allocate buffers if needed). *)
  val sizeof: t -> int

  (** Write to a buffered bigarray, at a given offset. *)
  val write: bufIO -> t -> unit

  (** Read a buffered bigarray at a given ofset. *)
  val read: bufIO -> t

  (** Set of elements *)
  module Set: SET with type elt = t

end

(** Keys *)
module type KEY = sig

  include BASE

  (** Compute the hash of a key. *)
  val hash: t -> int

  (** Compute a key from a raw string. *)
  val of_string: string -> t

  (** Convert a key to an hexa representation. *)
  val to_hex: t -> string

  (** Convert an hexa representation to a key. *)
  val of_hex: string -> t

  (** Compute a key from a list of keys. *)
  val concat: t list -> t

  (** Compute the key length. *)
  val length: t -> int

end

(** Values *)
module type VALUE = sig

  include BASE

  (** Type of keys. *)
  module Key: KEY

  (** Compute a key. *)
  val key: t -> Key.t

  (** Convert a raw string to a value. *)
  val of_string: string -> t

  (** Get the underlying raw blob. *)
  val to_string: t -> string option

  (** Create a new revision. *)
  val revision: Key.t -> Key.Set.t -> t

  (** Return the eventual contents key. *)
  val contents: t -> Key.t option

  (** Return the eventual parents. *)
  val parents: t -> Key.Set.t

  (** How to merge two values. Need to know how to merge keys. *)
  val merge: (Key.t -> Key.t -> Key.t option) -> t -> t -> t option

end

(** Tags *)
module type TAG = sig

  include BASE

  (** Convert a tag to a suitable name *)
  val to_string: t -> string

  (** Convert a name to a tag *)
  val of_string: string -> t

end

(** Ground consistent types. *)
module type CORE = sig

  (** Keys. *)
  module Key: KEY

  (** Values. *)
  module Value: VALUE with module Key = Key

  (** Tags. *)
  module Tag: TAG

end

(** {2 Stores} *)

(** The *key store* is graph of keys (where you can't remove vertex
    and edges). *)
module type KEY_STORE = sig

  (** Database handler. *)
  type t

  (** Core types. *)
  module C: CORE

  (** Add a key and its predecessors *)
  val add: t -> C.Key.t -> C.Key.Set.t -> unit Lwt.t

  (** Return all the available keys *)
  val all: t -> C.Key.Set.t Lwt.t

  (** Return the immediate predecessors *)
  val pred: t -> C.Key.t -> C.Key.Set.t Lwt.t

end

(** The *value store* is a low-level immutable and consistent
    key/value store. Deterministic computation of keys + immutability
    induces two very nice properties on the database structure:

    - if you modify a value, its computed key changes as well and you
    are creating a new key/value pair; and

    - if two stores share the same values, they will have the
    same keys: the overall structure of the data-store only depend on
    the stored data and not on any external user choices.
*)
module type VALUE_STORE = sig

  (** Database handler *)
  type t

  (** Core types. *)
  module C: CORE

  (** Add a value in the store. *)
  val write: t -> C.Value.t -> C.Key.t Lwt.t

  (** Read the value associated to a key. Return [None] if the key
       does not exist in the store. *)
  val read: t -> C.Key.t -> C.Value.t option Lwt.t

end

(** The *tag store* is a key/value store, where keys are names
    created by users (and/or global names created by convention) and
    values are keys from the low-level data-store. The tag data-store
    is neither immutable nor consistent, so it is very different from
    the low-level one.
*)
module type TAG_STORE = sig

  (** Database handler *)
  type t

  (** Core types. *)
  module C: CORE

  (** Update a tag. If the tag does not exist before, just create a
      new tag. *)
  val update: t -> C.Tag.t -> C.Key.Set.t -> unit Lwt.t

  (** Remove a tag. *)
  val remove: t -> C.Tag.t -> unit Lwt.t

  (** Read a tag. Return [None] if the tag does not exist in the
      store. *)
  val read: t -> C.Tag.t -> C.Key.Set.t Lwt.t

  (** List all the available tags *)
  val all: t -> C.Tag.Set.t Lwt.t

end

(** Global store. *)
module type STORE = sig

  (** Core types. *)
  module C: CORE

  (** Persist keys. *)
  module Key_store: KEY_STORE with module C = C

  (** Persist values. *)
  module Value_store: VALUE_STORE with module C = C

  (** Persists tags. *)
  module Tag_store: TAG_STORE with module C = C

  (** Abstract type for store. *)
  type t

  (** Projection to the key store handle. *)
  val key_store: t -> Key_store.t

  (** Projection to the value store handle. *)
  val value_store: t -> Value_store.t

  (** Projection to the tag store handle. *)
  val tag_store: t -> Tag_store.t

end

(** {1 Synchronization} *)

(** Signature for synchronization actions. *)
module type SYNC = sig

  (** Remote Irminsule instance. *)
  type t

  (** Core types. *)
  module C: CORE

  (** Graph of keys *)
  type graph = C.Key.Set.t * (C.Key.t * C.Key.t) list

  (** [pull_keys fd roots tags] pulls changes related to a given set
      known remote [tags]. Return the transitive closure of all the
      unknown keys, with [roots] as graph roots and [tags] as graph
      sinks. If [root] is the empty list, return the complete history
      up-to the given remote tags. *)
  val pull_keys: t -> C.Key.Set.t -> C.Tag.Set.t -> graph Lwt.t

  (** Get all the remote tags. *)
  val pull_tags: t -> (C.Tag.t * C.Key.Set.t) list Lwt.t

  (** Push changes related to a given (sub)-graph of keys, given set
      of local tags (which should belong to the graph). The does not
      modify the local tags on the remote instance. It is the user
      responsability to compute the smallest possible graph
      beforhand. *)
  val push_keys: t -> graph -> (C.Tag.t * C.Key.Set.t) list -> unit Lwt.t

  (** Modify the local tags of the remote instance. *)
  val push_tags: t -> (C.Tag.t * C.Key.Set.t) list -> unit Lwt.t

  (** Watch for changes for a given set of tags. Call a callback on
      each event ([tags] * [graphs]) where [tags] are the updated tags
      and [graph] the corresponding set of new keys (if any). *)
  val watch: t -> C.Tag.Set.t -> (C.Tag.Set.t -> graph -> unit Lwt.t) -> unit Lwt.t

end
