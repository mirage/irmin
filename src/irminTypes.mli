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

(** Base types *)
module type BASE = sig
  (** Abstract type *)
  type t

  (** Pretty-printing *)
  val pretty: t -> string

  (** Convert from JSON *)
  val of_json: IrminJSON.t -> t

  (** Convert to IrminJSON *)
  val to_json: t -> IrminJSON.t

end

(** Keys *)
module type KEY = sig

  include BASE

  (** Compare two keys *)
  val compare: t -> t -> int

  (** Hash a key *)
  val hash: t -> int

  (** Compute the key associated to any value. *)
  val create: 'a -> t

  (** Compute the key length *)
  val length: t -> int

end

(** Values *)
module type VALUE = BASE

(** Tags *)
module type TAG = BASE

(** {2 Stores} *)

(** The *key store* is graph of keys (where you can't remove vertex
    and edges). *)
module type KEY_STORE = sig

  (** Database handler *)
  type t

  (** Type of keys *)
  type key

  (** Add a key *)
  val add_key: t -> key -> unit Lwt.t

  (** Add a relation: [add_relation t k1 k2] means that [k1] is now a
      predecessor of [k2] in [t]. *)
  val add_relation: t -> key -> key -> unit Lwt.t

  (** Return the list of keys *)
  val list: t -> key list Lwt.t

  (** Return the predecessors *)
  val pred: t -> key -> key list Lwt.t

  (** Return the successors *)
  val succ: t -> key -> key list Lwt.t

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

  (** Type of keys *)
  type key

  (** Type of values *)
  type value

  (** Add a value in the store. *)
  val write: t -> value -> key Lwt.t

  (** Read the value associated to a key. Return [None] if the key
       does not exist in the store. *)
  val read: t -> key -> value option Lwt.t

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

  (** Type of tags *)
  type tag

  (** Type of keys *)
  type key

  (** Update a tag. If the tag does not exist before, just create a
      new tag. *)
  val update: t -> tag -> key -> unit Lwt.t

  (** Remove a tag. *)
  val remove: t -> tag -> unit Lwt.t

  (** Read a tag. Return [None] if the tag does not exist in the
      store. *)
  val read: t -> tag -> key option Lwt.t

  (** List all the available tags *)
  val list: t -> tag list Lwt.t

end

(** {2 Input/Outputs} *)

(** Channel abstraction *)
module type CHANNEL = sig

  (** Abstract type for channels *)
  type t

  (** Close a channel *)
  val close: t -> unit Lwt.t

  (** Read [n] bytes on a channel *)
  val read: t -> int -> Cstruct.t Lwt.t

  (** Read [n] bytes on a channel *)
  val read_string: t -> int -> string Lwt.t

  (** Write on a channel *)
  val write: t -> Cstruct.t -> unit Lwt.t

  (** Write on a channel *)
  val write_string: t -> string -> unit Lwt.t

end

(** [BASE] extended with IO capabilities *)
module type IO = sig

  (** Abstract type *)
  include BASE

  (** Abstract channel *)
  type channel

  (** Read from a channel *)
  val read: channel -> t Lwt.t

  (** Write to a channel *)
  val write: channel -> t -> unit Lwt.t

end

(** {1 Synchronization} *)

(** Signature for synchronization actions *)
module type SYNC = sig

  (** Abstract channel *)
  type t

  (** Type of keys *)
  type key

  (** Graph of keys *)
  type graph = key list * (key * key) list

  (** Type of remote tags *)
  type tag

  (** [pull_keys fd roots tags] pulls changes related to a given set
      known remote [tags]. Return the transitive closure of all the
      unknown keys, with [roots] as graph roots and [tags] as graph
      sinks. If [root] is the empty list, return the complete history
      up-to the given remote tags. *)
  val pull_keys: t -> key list -> tag list -> graph Lwt.t

  (** Get all the remote tags. *)
  val pull_tags: t -> (tag * key) list Lwt.t

  (** Push changes related to a given (sub)-graph of keys, given set
      of local tags (which should belong to the graph). The does not
      modify the local tags on the remote instance. It is the user
      responsability to compute the smallest possible graph
      beforhand. *)
  val push_keys: t -> graph -> (tag * key) list -> unit Lwt.t

  (** Modify the local tags of the remote instance. *)
  val push_tags: t -> (tag * key) list -> unit Lwt.t

  (** Watch for changes for a given set of tags. Call a callback on
      each event ([tags] * [graphs]) where [tags] are the updated tags
      and [graph] the corresponding set of new keys (if any). *)
  val watch: t -> tag list -> (tag list -> graph -> unit Lwt.t) -> unit Lwt.t

end

(** {2 Operations} *)
module type OPERATIONS = sig

  (** Type of channel *)
  type t

  (** Type of keys *)
  type key

  (** Type of values *)
  type value

  (** Type of tags *)
  type tag

  (** Key store *)
  module Key_store: KEY_STORE
    with type t = t
     and type key = key

  (** Value store *)
  module Value_store: VALUE_STORE
    with type t = t
     and type key = key
     and type value = value

  (** Tag store *)
  module Tag_store: TAG_STORE
    with type t = t
     and type key = key
     and type tag = tag

  (** Sync operations *)
  module Sync: SYNC
    with type t = t
     and type key = key
     and type tag = tag

end
