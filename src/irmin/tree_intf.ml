(*
 * Copyright (c) 2013-2021 Thomas Gazagnaire <thomas@gazagnaire.org>
 * Copyright (c) 2017 Grégoire Henry <gregoire.henry@ocamlpro.com>
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

open! Import

module type S = sig
  type key [@@deriving irmin]
  type step [@@deriving irmin]
  type metadata [@@deriving irmin]
  type contents [@@deriving irmin]
  type contents_key [@@deriving irmin]
  type node [@@deriving irmin]
  type hash [@@deriving irmin]

  (** [Tree] provides immutable, in-memory partial mirror of the store, with
      lazy reads and delayed writes.

      Trees are like staging area in Git: they are immutable temporary
      non-persistent areas (they disappear if the host crash), held in memory
      for efficiency, where reads are done lazily and writes are done only when
      needed on commit: if you modify a key twice, only the last change will be
      written to the store when you commit. *)

  type t [@@deriving irmin]
  (** The type of trees. *)

  (** {1 Constructors} *)

  val empty : unit -> t
  (** [empty ()] is the empty tree. The empty tree does not have associated
      backend configuration values, as they can perform in-memory operation,
      independently of any given backend. *)

  val singleton : key -> ?metadata:metadata -> contents -> t
  (** [singleton k c] is the tree with a single binding mapping the key [k] to
      the contents [c]. *)

  val of_contents : ?metadata:metadata -> contents -> t
  (** [of_contents c] is the subtree built from the contents [c]. *)

  val of_node : node -> t
  (** [of_node n] is the subtree built from the node [n]. *)

  type elt = [ `Node of node | `Contents of contents * metadata ]
  (** The type for tree elements. *)

  val v : elt -> t
  (** General-purpose constructor for trees. *)

  type kinded_hash = [ `Contents of hash * metadata | `Node of hash ]
  [@@deriving irmin]

  val pruned : kinded_hash -> t
  (** [pruned h] is a purely in-memory tree with the hash [h]. Such trees can be
      used as children of other in-memory tree nodes, for instance in order to
      compute the hash of the parent, but they cannot be dereferenced.

      Any operation that would require loading the contents of a pruned node
      (e.g. calling {!find} on one of its children) will instead raise a
      {!Pruned_hash} exception. Attempting to export a tree containing pruned
      sub-trees to a repository will fail similarly. *)

  val kind : t -> key -> [ `Contents | `Node ] option Lwt.t
  (** [kind t k] is the type of [s] in [t]. It could either be a tree node or
      some file contents. It is [None] if [k] is not present in [t]. *)

  val is_empty : t -> bool
  (** [is_empty t] is true iff [t] is {!empty} (i.e. a tree node with no
      children). Trees with {!kind} = [`Contents] are never considered empty. *)

  (** {1 Diffs} *)

  val diff : t -> t -> (key * (contents * metadata) Diff.t) list Lwt.t
  (** [diff x y] is the difference of contents between [x] and [y]. *)

  (** {1 Manipulating Contents} *)

  type 'a or_error = ('a, [ `Dangling_hash of hash ]) result
  (** Operations on lazy nodes can fail if the underlying store does not contain
      the expected hash. *)

  exception Dangling_hash of { context : string; hash : hash }
  (** The exception raised by functions that can force lazy tree nodes but do
      not return an explicit {!or_error}. *)

  exception Pruned_hash of { context : string; hash : hash }
  (** The exception raised by functions that attempt to load {!pruned} tree
      nodes. *)

  (** Operations on lazy tree contents. *)
  module Contents : sig
    type t
    (** The type of lazy tree contents. *)

    val hash : ?cache:bool -> t -> hash
    (** [hash t] is the hash of the {!contents} value returned when [t] is
        {!force}d successfully.

        {2 caching}

        [cache] regulates the caching behaviour regarding the node's internal
        data which are be lazily loaded from the backend.

        [cache] defaults to [true] which may greatly reduce the IOs and the
        runtime but may also grealy increase the memory consumption.

        [cache = false] doesn't replace a call to [clear], it only prevents the
        storing of new data, it doesn't discard the existing one. *)

    val key : t -> contents_key option
    (** [key t] is the key of the {!contents} value returned when [t] is
        {!force}d successfully. *)

    val force : t -> contents or_error Lwt.t
    (** [force t] forces evaluation of the lazy content value [t], or returns an
        error if no such value exists in the underlying repository. *)

    val force_exn : t -> contents Lwt.t
    (** Equivalent to {!force}, but raises an exception if the lazy content
        value is not present in the underlying repository. *)

    val clear : t -> unit
    (** [clear t] clears [t]'s cache. *)
  end

  val mem : t -> key -> bool Lwt.t
  (** [mem t k] is true iff [k] is associated to some contents in [t]. *)

  val find_all : t -> key -> (contents * metadata) option Lwt.t
  (** [find_all t k] is [Some (b, m)] if [k] is associated to the contents [b]
      and metadata [m] in [t] and [None] if [k] is not present in [t]. *)

  val length : node -> int Lwt.t
  (** [find n] is the number of entries in [n]. *)

  val find : t -> key -> contents option Lwt.t
  (** [find] is similar to {!find_all} but it discards metadata. *)

  val get_all : t -> key -> (contents * metadata) Lwt.t
  (** Same as {!find_all} but raise [Invalid_arg] if [k] is not present in [t]. *)

  val list :
    t ->
    ?offset:int ->
    ?length:int ->
    ?cache:bool ->
    key ->
    (step * t) list Lwt.t
  (** [list t key] is the list of files and sub-nodes stored under [k] in [t].
      The result order is not specified but is stable.

      [offset] and [length] are used for pagination.

      [cache] defaults to [true], see {!caching} for an explanation of the
      parameter. *)

  val get : t -> key -> contents Lwt.t
  (** Same as {!get_all} but ignore the metadata. *)

  val add : t -> key -> ?metadata:metadata -> contents -> t Lwt.t
  (** [add t k c] is the tree where the key [k] is bound to the contents [c] but
      is similar to [t] for other bindings. *)

  val update :
    t ->
    key ->
    ?metadata:metadata ->
    (contents option -> contents option) ->
    t Lwt.t
  (** [update t k f] is the tree [t'] that is the same as [t] for all keys
      except [k], and whose binding for [k] is determined by [f (find t k)].

      If [k] refers to an internal node of [t], [f] is called with [None] to
      determine the value with which to replace it. *)

  val remove : t -> key -> t Lwt.t
  (** [remove t k] is the tree where [k] bindings has been removed but is
      similar to [t] for other bindings. *)

  (** {1 Manipulating Subtrees} *)

  val mem_tree : t -> key -> bool Lwt.t
  (** [mem_tree t k] is false iff [find_tree k = None]. *)

  val find_tree : t -> key -> t option Lwt.t
  (** [find_tree t k] is [Some v] if [k] is associated to [v] in [t]. It is
      [None] if [k] is not present in [t]. *)

  val get_tree : t -> key -> t Lwt.t
  (** [get_tree t k] is [v] if [k] is associated to [v] in [t]. Raise
      [Invalid_arg] if [k] is not present in [t].*)

  val add_tree : t -> key -> t -> t Lwt.t
  (** [add_tree t k v] is the tree where the key [k] is bound to the non-empty
      tree [v] but is similar to [t] for other bindings.

      If [v] is empty, this is equivalent to [remove t k]. *)

  val update_tree : t -> key -> (t option -> t option) -> t Lwt.t
  (** [update_tree t k f] is the tree [t'] that is the same as [t] for all
      subtrees except under [k], and whose subtree at [k] is determined by
      [f (find_tree t k)].

      [f] returning either [None] or [Some empty] causes the subtree at [k] to
      be unbound (i.e. it is equivalent to [remove t k]). *)

  val merge : t Merge.t
  (** [merge] is the 3-way merge function for trees. *)

  (** {1 Folds} *)

  val destruct : t -> [ `Node of node | `Contents of Contents.t * metadata ]
  (** General-purpose destructor for trees. *)

  type marks
  (** The type for fold marks. *)

  val empty_marks : unit -> marks
  (** [empty_marks ()] is an empty collection of marks. *)

  type 'a force = [ `True | `False of key -> 'a -> 'a Lwt.t ]
  (** The type for {!fold}'s [force] parameter. [`True] forces the fold to read
      the objects of the lazy nodes and contents. [`False f] is applying [f] on
      every lazy node and content value instead. *)

  type uniq = [ `False | `True | `Marks of marks ]
  (** The type for {!fold}'s [uniq] parameters. [`False] folds over all the
      nodes. [`True] does not recurse on nodes already seen. [`Marks m] uses the
      collection of marks [m] to store the cache of keys: the fold will modify
      [m]. This can be used for incremental folds. *)

  type 'a node_fn = key -> step list -> 'a -> 'a Lwt.t
  (** The type for {!fold}'s [pre] and [post] parameters. *)

  type depth = [ `Eq of int | `Le of int | `Lt of int | `Ge of int | `Gt of int ]
  [@@deriving irmin]
  (** The type for fold depths.

      - [Eq d] folds over nodes and contents of depth exactly [d].
      - [Lt d] folds over nodes and contents of depth strictly less than [d].
      - [Gt d] folds over nodes and contents of depth strictly more than [d].

      [Le d] is [Eq d] and [Lt d]. [Ge d] is [Eq d] and [Gt d]. *)

  val fold :
    ?order:[ `Sorted | `Undefined | `Random of Random.State.t ] ->
    ?force:'a force ->
    ?cache:bool ->
    ?uniq:uniq ->
    ?pre:'a node_fn ->
    ?post:'a node_fn ->
    ?depth:depth ->
    ?contents:(key -> contents -> 'a -> 'a Lwt.t) ->
    ?node:(key -> node -> 'a -> 'a Lwt.t) ->
    ?tree:(key -> t -> 'a -> 'a Lwt.t) ->
    t ->
    'a ->
    'a Lwt.t
  (** [fold f t acc] folds [f] over [t]'s leafs.

      For every node [n], ui [n] is a leaf node, call [f path n]. Otherwise:

      - Call [pre path n]. By default [pre] is the identity;
      - Recursively call [fold] on each children.
      - Call [post path n]; By default [post] is the identity.

      See {!force} for details about the [force] parameters. By default it is
      [`True].

      See {!uniq} for details about the [uniq] parameters. By default it is
      [`False].

      The fold depth is controlled by the [depth] parameter.

      [cache] defaults to [false], see {!caching} for an explanation of the
      parameter.

      If [order] is [`Sorted] (the default), the elements are traversed in
      lexicographic order of their keys. If [`Random state], they are traversed
      in a random order. For large nodes, these two modes are memory-consuming,
      use [`Undefined] for a more memory efficient [fold]. *)

  (** {1 Stats} *)

  type stats = {
    nodes : int;  (** Number of node. *)
    leafs : int;  (** Number of leafs. *)
    skips : int;  (** Number of lazy nodes. *)
    depth : int;  (** Maximal depth. *)
    width : int;  (** Maximal width. *)
  }
  [@@deriving irmin]
  (** The type for tree stats. *)

  val stats : ?force:bool -> t -> stats Lwt.t
  (** [stats ~force t] are [t]'s statistics. If [force] is true, this will force
      the reading of lazy nodes. By default it is [false]. *)

  (** {1 Concrete Trees} *)

  type concrete =
    [ `Tree of (step * concrete) list | `Contents of contents * metadata ]
  [@@deriving irmin]
  (** The type for concrete trees. *)

  val of_concrete : concrete -> t
  (** [of_concrete c] is the subtree equivalent of the concrete tree [c].

      @raise Invalid_argument
        if [c] contains duplicate bindings for a given path. *)

  val to_concrete : t -> concrete Lwt.t
  (** [to_concrete t] is the concrete tree equivalent of the subtree [t]. *)

  (** {1 Caches} *)

  val clear : ?depth:int -> t -> unit
  (** [clear ?depth t] clears all caches in the tree [t] for subtrees with a
      depth higher than [depth]. If [depth] is not set, all of the subtrees are
      cleared.

      A call to [clear] doesn't discard the subtrees of [t], only their cache
      are discarded. Even the lazily loaded and unmodified subtrees remain. *)

  (** {1 Performance counters} *)

  type counters = {
    mutable contents_hash : int;
    mutable contents_find : int;
    mutable contents_add : int;
    mutable node_hash : int;
    mutable node_mem : int;
    mutable node_index : int;
    mutable node_add : int;
    mutable node_find : int;
    mutable node_val_v : int;
    mutable node_val_find : int;
    mutable node_val_list : int;
  }

  val counters : unit -> counters
  val dump_counters : unit Fmt.t
  val reset_counters : unit -> unit
  val inspect : t -> [ `Contents | `Node of [ `Map | `Key | `Value | `Pruned ] ]
end

module type Sigs = sig
  module type S = sig
    include S
    (** @inline *)
  end

  module Make (P : Backend.S) : sig
    include
      S
        with type key = P.Node.Path.t
         and type step = P.Node.Path.step
         and type metadata = P.Node.Metadata.t
         and type contents = P.Contents.value
         and type contents_key = P.Contents.Key.t
         and type hash = P.Hash.t

    type kinded_key =
      [ `Contents of P.Contents.Key.t * metadata | `Node of P.Node.Key.t ]
    [@@deriving irmin]

    val import : P.Repo.t -> kinded_key -> t option Lwt.t
    val import_no_check : P.Repo.t -> kinded_key -> t

    val export :
      ?clear:bool ->
      P.Repo.t ->
      [> write ] P.Contents.t ->
      [> read_write ] P.Node.t ->
      node ->
      P.Node.key Lwt.t

    val dump : t Fmt.t
    val equal : t -> t -> bool
    val key : t -> kinded_key option
    val hash : ?cache:bool -> t -> kinded_hash
    val to_backend_node : node -> P.Node.Val.t Lwt.t
    val to_backend_portable_node : node -> P.Node_portable.t Lwt.t
    val of_backend_node : P.Repo.t -> P.Node.value -> node
  end
end
