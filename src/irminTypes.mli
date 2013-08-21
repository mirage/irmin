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

(** Buffered IOs. It is a bigarray plus a blocking function which
    returns when the page containing the nth elements is ready.  *)
type bufIO = {
  buffer: (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t;
  ready: int -> unit Lwt.t;
  mutable offset: int;
}

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

  (** Size of serialized value (to pre-allocate buffers if needed) *)
  val sizeof: t -> int

  (** Write to a buffered bigarray, at a given offset. *)
  val write: bufIO -> t -> unit Lwt.t

  (** Read a buffered bigarray at a given ofset. *)
  val read: bufIO -> t Lwt.t

end

(** Keys *)
module type KEY = sig

  include BASE

  (** Compare two keys. *)
  val compare: t -> t -> int

  (** Hash a key. *)
  val hash: t -> int

  (** Are two keys equal *)
  val equal: t -> t -> bool

  (** Compute a key from a raw string. *)
  val of_string: string ->t

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

  (** Type of keys *)
  module Key: KEY

  (** Are two values equal ? *)
  val equal: t -> t -> bool

  (** Compute a key *)
  val key: t -> Key.t

  (** Convert a raw string to a value *)
  val blob: string -> t

  (** Return the predecessors *)
  val pred: t -> Key.t list

  (** How to merge two values. Need to know how to merge keys. *)
  val merge: (Key.t -> Key.t -> Key.t option) -> t -> t -> t option

end

(** Tags *)
module type TAG = sig

  include BASE

  (** Convert a tag to a suitable name *)
  val to_name: t -> string

  (** Convert a name to a tag *)
  val of_name: string -> t

end

(** {2 Stores} *)

(** The *key store* is graph of keys (where you can't remove vertex
    and edges). *)
module type KEY_STORE = sig

  (** Database handler *)
  type t

  (** Type of keys *)
  module Key: KEY

  (** Add a key *)
  val add_key: t -> Key.t -> unit Lwt.t

  (** Add a relation: [add_relation t k1 k2] means that [k1] is now a
      predecessor of [k2] in [t]. *)
  val add_relation: t -> Key.t -> Key.t -> unit Lwt.t

  (** Return the list of keys *)
  val list: t -> Key.t list Lwt.t

  (** Return the immediate predecessors *)
  val pred: t -> Key.t -> Key.t list Lwt.t

  (** Return the successors *)
  val succ: t -> Key.t -> Key.t list Lwt.t

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
  module Key: KEY

  (** Type of values *)
  module Value: VALUE with module Key = Key

  (** Add a value in the store. *)
  val write: t -> Value.t -> Key.t Lwt.t

  (** Read the value associated to a key. Return [None] if the key
       does not exist in the store. *)
  val read: t -> Key.t -> Value.t option Lwt.t

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
  module Tag: TAG

  (** Type of keys *)
  module Key: KEY

  (** Update a tag. If the tag does not exist before, just create a
      new tag. *)
  val update: t -> Tag.t -> Key.t -> unit Lwt.t

  (** Remove a tag. *)
  val remove: t -> Tag.t -> unit Lwt.t

  (** Read a tag. Return [None] if the tag does not exist in the
      store. *)
  val read: t -> Tag.t -> Key.t option Lwt.t

  (** List all the available tags *)
  val list: t -> Tag.t list Lwt.t

end

(** {1 Synchronization} *)

(** Signature for synchronization actions *)
module type SYNC = sig

  (** Remote Irminsule instannce *)
  type t

  (** Type of keys *)
  module Key: KEY

  (** Graph of keys *)
  type graph = Key.t list * (Key.t * Key.t) list

  (** Type of remote tags *)
  module Tag: TAG

  (** [pull_keys fd roots tags] pulls changes related to a given set
      known remote [tags]. Return the transitive closure of all the
      unknown keys, with [roots] as graph roots and [tags] as graph
      sinks. If [root] is the empty list, return the complete history
      up-to the given remote tags. *)
  val pull_keys: t -> Key.t list -> Tag.t list -> graph Lwt.t

  (** Get all the remote tags. *)
  val pull_tags: t -> (Tag.t * Key.t) list Lwt.t

  (** Push changes related to a given (sub)-graph of keys, given set
      of local tags (which should belong to the graph). The does not
      modify the local tags on the remote instance. It is the user
      responsability to compute the smallest possible graph
      beforhand. *)
  val push_keys: t -> graph -> (Tag.t * Key.t) list -> unit Lwt.t

  (** Modify the local tags of the remote instance. *)
  val push_tags: t -> (Tag.t * Key.t) list -> unit Lwt.t

  (** Watch for changes for a given set of tags. Call a callback on
      each event ([tags] * [graphs]) where [tags] are the updated tags
      and [graph] the corresponding set of new keys (if any). *)
  val watch: t -> Tag.t list -> (Tag.t list -> graph -> unit Lwt.t) -> unit Lwt.t

end
