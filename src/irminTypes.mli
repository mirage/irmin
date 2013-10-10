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

(** Base types *)
module type BASE = sig

  (** Abstract type *)
  type t

  (** Compare two elements. *)
  val compare: t -> t -> int

  (** Are two elements equal ? *)
  val equal: t -> t -> bool

  (** Compute the hash of an element. *)
  val hash: t -> int

  (** Pretty-printing *)
  val pretty: t -> string

  (** Convert from JSON *)
  val of_json: IrminJSON.t -> t

  (** Convert to IrminJSON *)
  val to_json: t -> IrminJSON.t

  (** Size of serialized value (to pre-allocate bufIO). *)
  val sizeof: t -> int

  (** Unmarshal from bufIO. *)
  val get: bufIO -> t

  (** Marshal to bufIO. *)
  val set: bufIO -> t -> unit

end

(** It's quite anoying to have to define that again ... *)
module type SET = sig

  (** Extend set's stdlib. *)
  include Set.S

  (** Convert a list to a set. *)
  val of_list: elt list -> t

  (** Convert a set to a list. *)
  val to_list: t -> elt list

  (** Implement the base operations. *)
  include BASE with type t := t

end

(** Base & set *)
module type BASESET = sig
  include BASE
  module Set: SET with type elt = t
end

(** Signature for graphs *)
module type GRAPH = sig

  (** Type of keys *)
  module Vertex: BASESET

  (** Mutable directed graph *)
  include Graph.Sig.I with type V.t = Vertex.t
  include Graph.Oper.S with type g := t

  (** Topoogical traversal *)
  module Topological: sig
    val fold: (V.t -> 'a -> 'a) -> t -> 'a -> 'a
  end

  (** Get all the vertices. *)
  val vertex: t -> Vertex.Set.t

  (** Get all the relations. *)
  val edges: t -> (vertex * vertex) list

  (** [make keys ~sources ~sinks pred ()] creates a graph contening
      at most the given keys, with the given [sources] and [sinks]. [pred]
      is used to compute the vertex relations of the resulting graph. *)
  val make: Vertex.Set.t
    -> ?sources:Vertex.Set.t -> ?sinks:Vertex.Set.t
    -> (Vertex.t -> Vertex.Set.t Lwt.t) -> t Lwt.t

  (** [dump g tags name] dumps the graph contents [g], which the
      vertices labelled by [tags]. [name] is the graph global label.  *)
  val dump: t ->
    ?labels:(vertex * string) list ->
    ?overlay:(vertex * vertex) list ->
    string -> unit

  (** Compute the minimum vertex. *)
  val min: t -> Vertex.Set.t

  (** Compute the maximun vertex. *)
  val max: t -> Vertex.Set.t

  (** Implements the base operations. *)
  include BASE with type t := t

end


(** Keys *)
module type KEY = sig

  include BASESET

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

  (** Graph of keys *)
  module Graph: GRAPH with type Vertex.t = t
                       and type Vertex.Set.t = Set.t

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

  (** [revision contents parents] creates a new revision with (a
      possibly empty) [contents] and the given [parents]. *)
  val revision: Key.t option -> Key.Set.t -> t

  (** Is a value actually a blob ? *)
  val is_blob: t -> bool

  (** Return the eventual contents key. *)
  val contents: t -> Key.t option

  (** Return the eventual parents. *)
  val parents: t -> Key.Set.t

  (** How to merge two values. Need to know how to merge keys. *)
  (* XXX: val merge: Key.t * t -> Key.t * t -> t option *)
  val merge: (Key.t -> Key.t -> Key.t option) -> t -> t -> t option

end

(** Tags *)
module type TAG = sig

  include BASESET

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

  (** [keys t ~sources ~sinks ()] returns the keys in the store. If
      [sources] is set, return only the keys greater than the given
      [sources]. If [sinks] is set, return only the key lesser than
      the given [sinks].*)
  val keys: t -> ?sources:C.Key.Set.t -> ?sinks:C.Key.Set.t -> unit
    -> C.Key.Graph.t Lwt.t

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

(** Event type *)
module type EVENT = sig

  module C: CORE

  (** Type of abstract events. *)
  type t =
    | Tag of (C.Tag.t * C.Key.Set.t)  (** A tag has been updated. *)
    | Graph of C.Key.Graph.t        (** New keys have been added. *)

  (** Base function on base events. *)
  include BASE with type t := t

end

(** Signature for synchronization actions. *)
module type SYNC = sig

  (** Remote Irminsule instance. *)
  type t

  (** Core types. *)
  module C: CORE

  (** [pull_keys fd ?sources ?sinks ()] pulls changes related to a
      given set known remote [sinks] tags. Return the transitive
      closure of all the unknown keys, with [sources] as graph sources
      and [sinks] as graph sinks. If [sources] is empty, return the
      complete history up-to the given remote tags. *)
  val pull_keys: t -> sources:C.Key.Set.t -> sinks:C.Tag.Set.t -> C.Key.Graph.t Lwt.t

  (** Get all the remote tags. *)
  val pull_tags: t -> (C.Tag.t * C.Key.Set.t) list Lwt.t

  (** Events. *)
  module Event: EVENT with module C = C

  (** Watch for changes for a given set of tags. Call a callback on
      each event ([tags] * [graphs]) where [tags] are the updated tags
      and [graph] the corresponding set of new keys (if any). *)
  val watch: t -> C.Tag.Set.t -> (Event.t -> unit Lwt.t) -> unit Lwt.t

end
