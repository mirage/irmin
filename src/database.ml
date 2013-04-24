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

module type TYPES = sig

  (** Database handler *)
  type t

  (** The type of keys used to lookup stored values. *)
  type key

  (** The type of values. *)
  type value

  (** Label on edges. *)
  type label

  (** Abstract type for tree-like filesystem. *)
  type tree

  (** Abstract type for revision values *)
  type revision

  (** Abstract type for tags. *)
  type tag

end

(** A generic key/value store *)
module type KV = sig

  type t
  type key
  type value

  (** Add a value in the store. Return the newly created key. *)
  val write: t -> value -> key

  (** Read the value associated to a key. Return [None] if nothing has
      been associated to the key yet. *)
  val read: t -> key -> value option

  (** List all the nodes. *)
  val list: t -> key list

end

(** Datamodel for Irminsule backends *)

(** A low-level immutable and consistent key/value data-store:

    - *immutable* (eg. append-only) means that if you modify a value,
    its computed key changes as well: hence you are creating a new
    key/value pair. Hence ; and

    - *consistent* means that if two data-stores share the same
    values, they will have the same keys: the overall structure of the
    data-store only depend on the stored data and not on any external
    user choices.

    A good candidate for the [value -> key] function is *SHA1* (but
    need to carrefuly take care of possible collisions).
*)
module type LOW = sig

  module T: TYPES
  open T

  include KV with type value := value
              and type key   := key
              and type t     := t

  (** Check whether a key is valid. *)
  val valid: t -> key -> bool

end

(** We use the low-level database to encoe a tree-like
    data-structure to model a *filesystem*, with two kinds of nodes:
    files nodes, which contain raw binary blobs; and directories nodes
    which contain the list of names, metadatas (such as permissions)
    and an ordered list of the keys of its sub-directories and
    sub-files.
*)
module type TREE = sig

  module T: TYPES
  open T

  include KV with type value := tree
              and type key   := key
              and type t     := t

  (** Get the node contents. *)
  val get: t -> tree -> label list -> key option

  (** Save a a value of type ['b] and all its modified parent nodes if
      needed. Return the the newly created tree root. *)
  val set: t -> tree -> label list -> value -> tree

  (** Implicit graph of nodes. *)
  val succ: t -> tree -> (label * tree) list

  (** Merge two trees by applying a merge function over each value
      stored at the same path in each tree (even if the values are
      equal). If the merge does not succeed for any reasons (such as
      merge conflict), it returns [None]. *)
  val merge: t -> (value option -> value option -> value option) ->
    tree -> tree -> tree option

    (* XXX: more expressive merge function *)
end

(** We also use the low-level database to encode a *partial-order* of
    filesystem revisions. A revisions is a node which contains
    metadata, the key of the filesystem root it is snapshoting and the
    list of keys of its immediate predecessors. This last part is
    encoding the Hasse relation of the partial order, which is
    equivalent to the full partial-order relation (as they have the
    same transitive closure). As the data-store is consistent, finding
    a common ancestor between two Irminsule instance is just a matter
    a finding a common snapshot key.
*)
module type REVISION = sig

  module T: TYPES
  open T

  include KV with type value := revision
              and type key   := key
              and type t     := t

  (** Return the predecessor of a given revision. *)
  val pred: t -> revision -> revision list

  (** Return the sub-structure pointed out by a given revision. *)
  val tree: t  -> revision -> tree option

  (** Commit a new tree as the child of a list of revisions, and
      get the newly created revision. *)
  val commit: t -> revision list -> tree -> revision

end

(** The *tag data-store* is a key/value store, where keys are
    names created by users (and/or global names created by convention) and
    values are keys from the low-level data-store. The tag data-store is
    neither immutable nor consistent, so it is very different from the
    low-level one. *)
module type TAG = sig

  module T: TYPES
  open T

  (** Get the list of tags *)
  val tags: t -> tag list

  (** Read a tag *)
  val revision: t -> tag -> revision option

  (** Write a new tag *)
  val tag: t -> tag -> revision -> unit

end

(** Remote actions. *)
module type REMOTE = sig

  module T: TYPES
  open T

  (** [discover current tags] return the diff of revisions between the
      [current] state of what we known from world and the state of
      [tags] in the remote repository. Return the missing repository
      keys. *)
  val discover: t -> revision list -> tag list -> key list

  (** Pull values. The order is kept between the received keys and the
      sent values. *)
  val pull: t -> key list -> value list

  (** Push values *)
  val push: t -> value list -> unit

  (** Watch for changes in a substree. Return the new subtree keys. *)
  val watch: t -> tag -> label list -> key list

end
