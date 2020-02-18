(*
 * Copyright (c) 2013-2017 Thomas Gazagnaire <thomas@gazagnaire.org>
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

(** Irmin signatures *)

module type PATH = sig
  (** {1 Path} *)

  type t
  (** The type for path values. *)

  type step
  (** Type type for path's steps. *)

  val empty : t
  (** The empty path. *)

  val v : step list -> t
  (** Create a path from a list of steps. *)

  val is_empty : t -> bool
  (** Check if the path is empty. *)

  val cons : step -> t -> t
  (** Prepend a step to the path. *)

  val rcons : t -> step -> t
  (** Append a step to the path. *)

  val decons : t -> (step * t) option
  (** Deconstruct the first element of the path. Return [None] if the path is
      empty. *)

  val rdecons : t -> (t * step) option
  (** Deconstruct the last element of the path. Return [None] if the path is
      empty. *)

  val map : t -> (step -> 'a) -> 'a list
  (** [map t f] maps [f] over all steps of [t]. *)

  (** {1 Value Types} *)

  val t : t Type.t
  (** [t] is the value type for {!t}. *)

  val step_t : step Type.t
  (** [step_t] is the value type for {!step}. *)
end

module type HASH = sig
  (** Signature for digest hashes, inspired by Digestif. *)

  type t
  (** The type for digest hashes. *)

  val hash : ((string -> unit) -> unit) -> t
  (** Compute a deterministic store key from a sequence of strings. *)

  val short_hash : t -> int
  (** [short_hash h] is a small hash of [h], to be used for instance as the
      `hash` function of an OCaml [Hashtbl]. *)

  val hash_size : int
  (** [hash_size] is the size of hash results, in bytes. *)

  (** {1 Value Types} *)

  val t : t Type.t
  (** [t] is the value type for {!t}. *)
end

module type TYPED_HASH = sig
  type t

  type value

  val hash : value -> t
  (** Compute a deterministic store key from a string. *)

  val short_hash : t -> int
  (** [short_hash h] is a small hash of [h], to be used for instance as the
      `hash` function of an OCaml [Hashtbl]. *)

  val hash_size : int
  (** [hash_size] is the size of hash results, in bytes. *)

  (** {1 Value Types} *)

  val t : t Type.t
  (** [t] is the value type for {!t}. *)
end

module type CONTENTS = sig
  (** {1 Signature for store contents} *)

  type t
  (** The type for user-defined contents. *)

  val t : t Type.t
  (** [t] is the value type for {!t}. *)

  val merge : t option Merge.t
  (** Merge function. Evaluates to [`Conflict msg] if the values cannot be
      merged properly. The arguments of the merge function can take [None] to
      mean that the key does not exists for either the least-common ancestor or
      one of the two merging points. The merge function returns [None] when the
      key's value should be deleted. *)
end

module type CONTENT_ADDRESSABLE_STORE = sig
  (** {1 Content-addressable stores}

      Content-addressable stores are store where it is possible to read and add
      new values. Keys are derived from the values raw contents and hence are
      deterministic. *)

  type 'a t
  (** The type for content-addressable backend stores. The ['a] phantom type
      carries information about the store mutability. *)

  type key
  (** The type for keys. *)

  type value
  (** The type for raw values. *)

  val mem : [> `Read ] t -> key -> bool Lwt.t
  (** [mem t k] is true iff [k] is present in [t]. *)

  val find : [> `Read ] t -> key -> value option Lwt.t
  (** [find t k] is [Some v] if [k] is associated to [v] in [t] and [None] is
      [k] is not present in [t]. *)

  val add : [> `Write ] t -> value -> key Lwt.t
  (** Write the contents of a value to the store. It's the responsibility of the
      content-addressable store to generate a consistent key. *)

  val unsafe_add : [> `Write ] t -> key -> value -> unit Lwt.t
  (** Same as {!add} but allows to specify the key directly. The backend might
      choose to discared that key and/or can be corrupt if the key scheme is not
      consistent. *)
end

module type CONTENT_ADDRESSABLE_STORE_MAKER = functor
  (K : HASH)
  (V : Type.S)
  -> sig
  include CONTENT_ADDRESSABLE_STORE with type key = K.t and type value = V.t

  val batch : [ `Read ] t -> ([ `Read | `Write ] t -> 'a Lwt.t) -> 'a Lwt.t

  val v : Conf.t -> [ `Read ] t Lwt.t

  val close : 'a t -> unit Lwt.t
end

module type APPEND_ONLY_STORE = sig
  (** {1 Append-only stores}

      Append-onlye stores are store where it is possible to read and add new
      values. *)

  type 'a t
  (** The type for append-only backend stores. The ['a] phantom type carries
      information about the store mutability. *)

  type key
  (** The type for keys. *)

  type value
  (** The type for raw values. *)

  val mem : [> `Read ] t -> key -> bool Lwt.t
  (** [mem t k] is true iff [k] is present in [t]. *)

  val find : [> `Read ] t -> key -> value option Lwt.t
  (** [find t k] is [Some v] if [k] is associated to [v] in [t] and [None] is
      [k] is not present in [t]. *)

  val add : [> `Write ] t -> key -> value -> unit Lwt.t
  (** Write the contents of a value to the store. *)
end

module type APPEND_ONLY_STORE_MAKER = functor (K : Type.S) (V : Type.S) -> sig
  include APPEND_ONLY_STORE with type key = K.t and type value = V.t

  val batch : [ `Read ] t -> ([ `Read | `Write ] t -> 'a Lwt.t) -> 'a Lwt.t

  val v : Conf.t -> [ `Read ] t Lwt.t

  val close : 'a t -> unit Lwt.t
end

module type METADATA = sig
  type t
  (** The type for metadata. *)

  val t : t Type.t
  (** [t] is the value type for {!t}. *)

  val merge : t Merge.t
  (** [merge] is the merge function for metadata. *)

  val default : t
  (** The default metadata to attach, for APIs that don't care about metadata. *)
end

module type CONTENTS_STORE = sig
  include CONTENT_ADDRESSABLE_STORE

  val merge : [ `Read | `Write ] t -> key option Merge.t
  (** [merge t] lifts the merge functions defined on contents values to contents
      key. The merge function will: {e (i)} read the values associated with the
      given keys, {e (ii)} use the merge function defined on values and {e
      (iii)} write the resulting values into the store to get the resulting key.
      See {!Contents.S.merge}.

      If any of these operations fail, return [`Conflict]. *)

  (** [Key] provides base functions for user-defined contents keys. *)
  module Key : TYPED_HASH with type t = key and type value = value

  module Val : CONTENTS with type t = value
  (** [Val] provides base functions for user-defined contents values. *)
end

module type NODE = sig
  (** {1 Node values} *)

  type t
  (** The type for node values. *)

  type metadata
  (** The type for node metadata. *)

  type hash
  (** The type for keys. *)

  type step
  (** The type for steps between nodes. *)

  type value = [ `Node of hash | `Contents of hash * metadata ]
  (** The type for either (node) keys or (contents) keys combined with their
      metadata. *)

  val v : (step * value) list -> t
  (** [create l] is a new node. *)

  val list : t -> (step * value) list
  (** [list t] is the contents of [t]. *)

  val empty : t
  (** [empty] is the empty node. *)

  val is_empty : t -> bool
  (** [is_empty t] is true iff [t] is {!empty}. *)

  val find : t -> step -> value option
  (** [find t s] is the value associated with [s] in [t].

      A node can point to user-defined {{!Node.S.contents} contents}. The edge
      between the node and the contents is labeled by a {{!Node.S.step} step}. *)

  val add : t -> step -> value -> t
  (** [add t s v] is the node where [find t v] is [Some s] but is similar to [t]
      otherwise. *)

  val remove : t -> step -> t
  (** [remove t s] is the node where [find t s] is [None] but is similar to [t]
      otherwise. *)

  (** {1 Value types} *)

  val t : t Type.t
  (** [t] is the value type for {!t}. *)

  val default : metadata
  (** [default] is the default metadata value. *)

  val metadata_t : metadata Type.t
  (** [metadata_t] is the value type for {!metadata}. *)

  val hash_t : hash Type.t
  (** [hash_t] is the value type for {!hash}. *)

  val step_t : step Type.t
  (** [step_t] is the value type for {!step}. *)

  val value_t : value Type.t
  (** [value_t] is the value type for {!value}. *)
end

module type NODE_GRAPH = sig
  (** {1 Node Graphs} *)

  type 'a t
  (** The type for store handles. *)

  type metadata
  (** The type for node metadata. *)

  type contents
  (** The type of user-defined contents. *)

  type node
  (** The type for node values. *)

  type step
  (** The type of steps. A step is used to pass from one node to another. *)

  type path
  (** The type of store paths. A path is composed of {{!step} steps}. *)

  type value = [ `Node of node | `Contents of contents * metadata ]
  (** The type for store values. *)

  val empty : [> `Write ] t -> node Lwt.t
  (** The empty node. *)

  val v : [> `Write ] t -> (step * value) list -> node Lwt.t
  (** [v t n] is a new node containing [n]. *)

  val list : [> `Read ] t -> node -> (step * value) list Lwt.t
  (** [list t n] is the contents of the node [n]. *)

  val find : [> `Read ] t -> node -> path -> value option Lwt.t
  (** [find t n p] is the contents of the path [p] starting form [n]. *)

  val add : [ `Read | `Write ] t -> node -> path -> value -> node Lwt.t
  (** [add t n p v] is the node [x] such that [find t x p] is [Some v] and it
      behaves the same [n] for other operations. *)

  val remove : [ `Read | `Write ] t -> node -> path -> node Lwt.t
  (** [remove t n path] is the node [x] such that [find t x] is [None] and it
      behhaves then same as [n] for other operations. *)

  val closure :
    [> `Read ] t -> min:node list -> max:node list -> node list Lwt.t
  (** [closure t min max] is the unordered list of nodes [n] reachable from a
      node of [max] along a path which: (i) either contains no [min] or (ii) it
      ends with a [min].

      {b Note:} Both [min] and [max] are subsets of [n]. *)

  val iter :
    [> `Read ] t ->
    min:node list ->
    max:node list ->
    ?node:(node -> unit Lwt.t) ->
    ?edge:(node -> node -> unit Lwt.t) ->
    ?skip:(node -> bool Lwt.t) ->
    ?rev:bool ->
    unit ->
    unit Lwt.t
  (** [iter min max node edge skip rev ()] iterates in topological order over
      the closure of [t] as specified by {{!Private.Node.GRAPH.closure}
      GRAPH.closure}.

      It applies three functions while traversing the graph: [node] on the
      nodes; [edge n predecessor_of_n] on the directed edges and [skip n] to not
      include a node [n], its predecessors and the outgoing edges of [n].

      If [rev] is true (the default) then the graph is traversed in the reverse
      order: [node n] is applied only after it was applied on all its
      predecessors; [edge n p] is applied after [node n]. Note that [edge n p]
      is applied even if [p] is skipped. *)

  (** {1 Value Types} *)

  val metadata_t : metadata Type.t
  (** [metadat_t] is the value type for {!metadata}. *)

  val contents_t : contents Type.t
  (** [contents_t] is the value type for {!contents}. *)

  val node_t : node Type.t
  (** [node_t] is the value type for {!node}. *)

  val step_t : step Type.t
  (** [step_t] is the value type for {!step}. *)

  val path_t : path Type.t
  (** [path_t] is the value type for {!path}. *)

  val value_t : value Type.t
  (** [value_t] is the value type for {!value}. *)
end

module type NODE_STORE = sig
  include CONTENT_ADDRESSABLE_STORE

  module Path : PATH
  (** [Path] provides base functions on node paths. *)

  val merge : [ `Read | `Write ] t -> key option Merge.t
  (** [merge] is the 3-way merge function for nodes keys. *)

  (** [Key] provides base functions for node keys. *)
  module Key : TYPED_HASH with type t = key and type value = value

  module Metadata : METADATA
  (** [Metadata] provides base functions for node metadata. *)

  (** [Val] provides base functions for node values. *)
  module Val :
    NODE
      with type t = value
       and type hash = key
       and type metadata = Metadata.t
       and type step = Path.step

  module Contents : CONTENTS_STORE with type key = Val.hash
  (** [Contents] is the underlying contents store. *)
end

type config = Conf.t

type 'a diff = 'a Diff.t

module type COMMIT = sig
  (** {1 Commit values} *)

  type t
  (** The type for commit values. *)

  type hash
  (** Type for keys. *)

  val v : info:Info.t -> node:hash -> parents:hash list -> t
  (** Create a commit. *)

  val node : t -> hash
  (** The underlying node. *)

  val parents : t -> hash list
  (** The commit parents. *)

  val info : t -> Info.t
  (** The commit info. *)

  (** {1 Value Types} *)

  val t : t Type.t
  (** [t] is the value type for {!t}. *)

  val hash_t : hash Type.t
  (** [hash_t] is the value type for {!hash}. *)
end

module type COMMIT_STORE = sig
  (** {1 Commit Store} *)

  include CONTENT_ADDRESSABLE_STORE

  val merge : [ `Read | `Write ] t -> info:Info.f -> key option Merge.t
  (** [merge] is the 3-way merge function for commit keys. *)

  (** [Key] provides base functions for commit keys. *)
  module Key : TYPED_HASH with type t = key and type value = value

  (** [Val] provides functions for commit values. *)
  module Val : COMMIT with type t = value and type hash = key

  module Node : NODE_STORE with type key = Val.hash
  (** [Node] is the underlying node store. *)
end

module type COMMIT_HISTORY = sig
  (** {1 Commit History} *)

  type 'a t
  (** The type for store handles. *)

  type node
  (** The type for node values. *)

  type commit
  (** The type for commit values. *)

  type v
  (** The type for commit objects. *)

  val v :
    [> `Write ] t ->
    node:node ->
    parents:commit list ->
    info:Info.t ->
    (commit * v) Lwt.t
  (** Create a new commit. *)

  val parents : [> `Read ] t -> commit -> commit list Lwt.t
  (** Get the commit parents.

      Commits form a append-only, fully functional, partial-order
      data-structure: every commit carries the list of its immediate
      predecessors. *)

  val merge : [ `Read | `Write ] t -> info:Info.f -> commit Merge.t
  (** [merge t] is the 3-way merge function for commit. *)

  val lcas :
    [> `Read ] t ->
    ?max_depth:int ->
    ?n:int ->
    commit ->
    commit ->
    (commit list, [ `Max_depth_reached | `Too_many_lcas ]) result Lwt.t
  (** Find the lowest common ancestors
      {{:http://en.wikipedia.org/wiki/Lowest_common_ancestor} lca} between two
      commits. *)

  val lca :
    [ `Read | `Write ] t ->
    info:Info.f ->
    ?max_depth:int ->
    ?n:int ->
    commit list ->
    (commit option, Merge.conflict) result Lwt.t
  (** Compute the lowest common ancestors ancestor of a list of commits by
      recursively calling {!lcas} and merging the results.

      If one of the merges results in a conflict, or if a call to {!lcas}
      returns either [Error `Max_depth_reached] or [Error `Too_many_lcas] then
      the function returns the same error. *)

  val three_way_merge :
    [ `Read | `Write ] t ->
    info:Info.f ->
    ?max_depth:int ->
    ?n:int ->
    commit ->
    commit ->
    (commit, Merge.conflict) result Lwt.t
  (** Compute the {!lcas} of the two commit and 3-way merge the result. *)

  val closure :
    [> `Read ] t -> min:commit list -> max:commit list -> commit list Lwt.t
  (** Same as {{!Private.Node.GRAPH.closure} GRAPH.closure} but for the history
      graph. *)

  (** {1 Value Types} *)

  val commit_t : commit Type.t
  (** [commit_t] is the value type for {!commit}. *)
end

module type SLICE = sig
  (** {1 Slices} *)

  type t
  (** The type for slices. *)

  type contents
  (** The type for exported contents. *)

  type node
  (** The type for exported nodes. *)

  type commit
  (** The type for exported commits. *)

  type value = [ `Contents of contents | `Node of node | `Commit of commit ]
  (** The type for exported values. *)

  val empty : unit -> t Lwt.t
  (** Create a new empty slice. *)

  val add : t -> value -> unit Lwt.t
  (** [add t v] adds [v] to [t]. *)

  val iter : t -> (value -> unit Lwt.t) -> unit Lwt.t
  (** [iter t f] calls [f] on all values of [t]. *)

  (** {1 Value Types} *)

  val t : t Type.t
  (** [t] is the value type for {!t}. *)

  val contents_t : contents Type.t
  (** [content_t] is the value type for {!contents}. *)

  val node_t : node Type.t
  (** [node_t] is the value type for {!node}. *)

  val commit_t : commit Type.t
  (** [commit_t] is the value type for {!commit}. *)

  val value_t : value Type.t
  (** [value_t] is the value type for {!value}. *)
end

module type BRANCH = sig
  (** {1 Signature for Branches} *)

  type t
  (** The type for branches. *)

  val t : t Type.t
  (** [t] is the value type for {!t}. *)

  val master : t
  (** The name of the master branch. *)

  val is_valid : t -> bool
  (** Check if the branch is valid. *)
end

module type ATOMIC_WRITE_STORE = sig
  (** {1 Atomic write stores}

      Atomic-write stores are stores where it is possible to read, update and
      remove elements, with atomically guarantees. *)

  type t
  (** The type for atomic-write backend stores. *)

  type key
  (** The type for keys. *)

  type value
  (** The type for raw values. *)

  val mem : t -> key -> bool Lwt.t
  (** [mem t k] is true iff [k] is present in [t]. *)

  val find : t -> key -> value option Lwt.t
  (** [find t k] is [Some v] if [k] is associated to [v] in [t] and [None] is
      [k] is not present in [t]. *)

  val set : t -> key -> value -> unit Lwt.t
  (** [set t k v] replaces the contents of [k] by [v] in [t]. If [k] is not
      already defined in [t], create a fresh binding. Raise [Invalid_argument]
      if [k] is the {{!Path.empty} empty path}. *)

  val test_and_set :
    t -> key -> test:value option -> set:value option -> bool Lwt.t
  (** [test_and_set t key ~test ~set] sets [key] to [set] only if the current
      value of [key] is [test] and in that case returns [true]. If the current
      value of [key] is different, it returns [false]. [None] means that the
      value does not have to exist or is removed.

      {b Note:} The operation is guaranteed to be atomic. *)

  val remove : t -> key -> unit Lwt.t
  (** [remove t k] remove the key [k] in [t]. *)

  val list : t -> key list Lwt.t
  (** [list t] it the list of keys in [t]. *)

  type watch
  (** The type of watch handlers. *)

  val watch :
    t ->
    ?init:(key * value) list ->
    (key -> value diff -> unit Lwt.t) ->
    watch Lwt.t
  (** [watch t ?init f] adds [f] to the list of [t]'s watch handlers and returns
      the watch handler to be used with {!unwatch}. [init] is the optional
      initial values. It is more efficient to use {!watch_key} to watch only a
      single given key.*)

  val watch_key :
    t -> key -> ?init:value -> (value diff -> unit Lwt.t) -> watch Lwt.t
  (** [watch_key t k ?init f] adds [f] to the list of [t]'s watch handlers for
      the key [k] and returns the watch handler to be used with {!unwatch}.
      [init] is the optional initial value of the key. *)

  val unwatch : t -> watch -> unit Lwt.t
  (** [unwatch t w] removes [w] from [t]'s watch handlers. *)

  val close : t -> unit Lwt.t
  (** [close t] frees up all the resources associated to [t]. Any operations run
      on a closed store will raise {!Closed}. *)
end

module type ATOMIC_WRITE_STORE_MAKER = functor (K : Type.S) (V : Type.S) -> sig
  include ATOMIC_WRITE_STORE with type key = K.t and type value = V.t

  val v : Conf.t -> t Lwt.t
end

module type BRANCH_STORE = sig
  (** {1 Branch Store} *)

  include ATOMIC_WRITE_STORE

  module Key : BRANCH with type t = key
  (** Base functions on keys. *)

  module Val : HASH with type t = value
  (** Base functions on values. *)
end

type remote = ..

module type SYNC = sig
  (** {1 Remote synchronization} *)

  type t
  (** The type for store handles. *)

  type commit
  (** The type for store heads. *)

  type branch
  (** The type for branch IDs. *)

  type endpoint
  (** The type for sync endpoints. *)

  val fetch :
    t ->
    ?depth:int ->
    endpoint ->
    branch ->
    (commit option, [ `Msg of string ]) result Lwt.t
  (** [fetch t uri] fetches the contents of the remote store located at [uri]
      into the local store [t]. Return the head of the remote branch with the
      same name, which is now in the local store. [No_head] means no such branch
      exists. *)

  val push :
    t ->
    ?depth:int ->
    endpoint ->
    branch ->
    (unit, [ `Msg of string | `Detached_head ]) result Lwt.t
  (** [push t uri] pushes the contents of the local store [t] into the remote
      store located at [uri]. *)
end

module type PRIVATE = sig
  module Hash : HASH

  module Contents : CONTENTS_STORE with type key = Hash.t

  module Node :
    NODE_STORE with type key = Hash.t and type Val.hash = Contents.key

  module Commit :
    COMMIT_STORE with type key = Hash.t and type Val.hash = Node.key

  module Branch : BRANCH_STORE with type value = Commit.key

  module Slice :
    SLICE
      with type contents = Contents.key * Contents.value
       and type node = Node.key * Node.value
       and type commit = Commit.key * Commit.value

  module Repo : sig
    type t

    val v : Conf.t -> t Lwt.t

    val close : t -> unit Lwt.t

    val contents_t : t -> [ `Read ] Contents.t

    val node_t : t -> [ `Read ] Node.t

    val commit_t : t -> [ `Read ] Commit.t

    val branch_t : t -> Branch.t

    val batch :
      t ->
      ([ `Read | `Write ] Contents.t ->
      [ `Read | `Write ] Node.t ->
      [ `Read | `Write ] Commit.t ->
      'a Lwt.t) ->
      'a Lwt.t
  end

  module Sync : sig
    include SYNC with type commit = Commit.key and type branch = Branch.key

    val v : Repo.t -> t Lwt.t
  end
end

module type TREE = sig
  type key

  type step

  type metadata

  type contents

  type node

  type tree = [ `Node of node | `Contents of contents * metadata ]

  (** [Tree] provides immutable, in-memory partial mirror of the store, with
      lazy reads and delayed writes.

      Trees are like staging area in Git: they are immutable temporary
      non-persistent areas (they disappear if the host crash), held in memory
      for efficiency, where reads are done lazily and writes are done only when
      needed on commit: if you modify a key twice, only the last change will be
      written to the store when you commit. *)

  (** {1 Constructors} *)

  val empty : tree
  (** [empty] is the empty tree. The empty tree does not have associated backend
      configuration values, as they can perform in-memory operation,
      independently of any given backend. *)

  val of_contents : ?metadata:metadata -> contents -> tree
  (** [of_contents c] is the subtree built from the contents [c]. *)

  val of_node : node -> tree
  (** [of_node n] is the subtree built from the node [n]. *)

  val kind : tree -> key -> [ `Contents | `Node ] option Lwt.t
  (** [kind t k] is the type of [s] in [t]. It could either be a tree node or
      some file contents. It is [None] if [k] is not present in [t]. *)

  val list : tree -> key -> (step * [ `Contents | `Node ]) list Lwt.t
  (** [list t key] is the list of files and sub-nodes stored under [k] in [t]. *)

  (** {1 Diffs} *)

  val diff : tree -> tree -> (key * (contents * metadata) diff) list Lwt.t
  (** [diff x y] is the difference of contents between [x] and [y]. *)

  (** {1 Manipulating Contents} *)

  val mem : tree -> key -> bool Lwt.t
  (** [mem t k] is true iff [k] is associated to some contents in [t]. *)

  val find_all : tree -> key -> (contents * metadata) option Lwt.t
  (** [find_all t k] is [Some (b, m)] if [k] is associated to the contents [b]
      and metadata [m] in [t] and [None] if [k] is not present in [t]. *)

  val find : tree -> key -> contents option Lwt.t
  (** [find] is similar to {!find_all} but it discards metadata. *)

  val get_all : tree -> key -> (contents * metadata) Lwt.t
  (** Same as {!find_all} but raise [Invalid_arg] if [k] is not present in [t]. *)

  val get : tree -> key -> contents Lwt.t
  (** Same as {!get_all} but ignore the metadata. *)

  val add : tree -> key -> ?metadata:metadata -> contents -> tree Lwt.t
  (** [add t k c] is the tree where the key [k] is bound to the contents [c] but
      is similar to [t] for other bindings. *)

  val remove : tree -> key -> tree Lwt.t
  (** [remove t k] is the tree where [k] bindings has been removed but is
      similar to [t] for other bindings. *)

  (** {1 Manipulating Subtrees} *)

  val mem_tree : tree -> key -> bool Lwt.t
  (** [mem_tree t k] is false iff [find_tree k = None]. *)

  val find_tree : tree -> key -> tree option Lwt.t
  (** [find_tree t k] is [Some v] if [k] is associated to [v] in [t]. It is
      [None] if [k] is not present in [t]. *)

  val get_tree : tree -> key -> tree Lwt.t
  (** [get_tree t k] is [v] if [k] is associated to [v] in [t]. Raise
      [Invalid_arg] if [k] is not present in [t].*)

  val add_tree : tree -> key -> tree -> tree Lwt.t
  (** [add_tree t k v] is the tree where the key [k] is bound to the tree [v]
      but is similar to [t] for other bindings *)

  val merge : tree Merge.t
  (** [merge] is the 3-way merge function for trees. *)

  (** {1 Folds} *)

  type marks
  (** The type for fold marks. *)

  val empty_marks : unit -> marks
  (** [empty_marks ()] is an empty collection of marks. *)

  type 'a force = [ `True | `False of key -> 'a -> 'a Lwt.t ]
  (** The type for {!fold}'s [force] parameter. [`True] forces the fold to read
      the objects of the lazy nodes. [`False f] is applying [f] on every lazy
      node instead. *)

  type uniq = [ `False | `True | `Marks of marks ]
  (** The type for {!fold}'s [uniq] parameters. [`False] folds over all the
      nodes. [`True] does not recurse on nodes already seen. [`Marks m] uses the
      collection of marks [m] to store the cache of keys: the fold will modify
      [m]. This can be used for incremental folds. *)

  type 'a node_fn = key -> step list -> 'a -> 'a Lwt.t
  (** The type for {!fold}'s [pre] and [post] parameters. *)

  val fold :
    ?force:'a force ->
    ?uniq:uniq ->
    ?pre:'a node_fn ->
    ?post:'a node_fn ->
    (key -> contents -> 'a -> 'a Lwt.t) ->
    tree ->
    'a ->
    'a Lwt.t
  (** [fold f t acc] folds [f] over [t]'s leafs.

      For every node [n], ui [n] is a leaf node, call [f path n]. Otherwise:

      - Call [pre path n]. By default [pre] is the identity;
      - Recursively call [fold] on each children, in lexicographic order;
      - Call [post path n]; By default [post] is the identity.

      See {!force} for details about the [force] parameters. By default it is
      [`True].

      See {!uniq} for details about the [uniq] parameters. By default it is
      [`False]. *)

  (** {1 Stats} *)

  type stats = {
    nodes : int;  (** Number of node. *)
    leafs : int;  (** Number of leafs. *)
    skips : int;  (** Number of lazy nodes. *)
    depth : int;  (** Maximal depth. *)
    width : int;  (** Maximal width. *)
  }
  (** The type for tree stats. *)

  val pp_stats : stats Fmt.t
  (** [pp_stats] is the pretty printer for tree statistics. *)

  val stats : ?force:bool -> tree -> stats Lwt.t
  (** [stats ~force t] are [t]'s statistics. If [force] is true, this will force
      the reading of lazy nodes. By default it is [false]. *)

  (** {1 Concrete Trees} *)

  type concrete =
    [ `Tree of (step * concrete) list | `Contents of contents * metadata ]
  (** The type for concrete trees. *)

  val of_concrete : concrete -> tree
  (** [of_concrete c] is the subtree equivalent to the concrete tree [c]. *)

  val to_concrete : tree -> concrete Lwt.t
  (** [to_concrete t] is the concrete tree equivalent to the subtree [t]. *)

  (** {1 Caches} *)

  val clear : ?depth:int -> tree -> unit
  (** [clear ?depth t] clears all the cache in the tree [t] for subtrees with a
      depth higher than [depth]. If [depth] is not set, all the subtrees are
      cleared. *)

  (** {1 Performance counters} *)

  type counters = {
    mutable contents_hash : int;
    mutable contents_find : int;
    mutable contents_add : int;
    mutable node_hash : int;
    mutable node_mem : int;
    mutable node_add : int;
    mutable node_find : int;
    mutable node_val_v : int;
    mutable node_val_find : int;
    mutable node_val_list : int;
  }

  val counters : unit -> counters

  val dump_counters : unit Fmt.t

  val reset_counters : unit -> unit

  val inspect : tree -> [ `Contents | `Node of [ `Map | `Hash | `Value ] ]
end

module type STORE = sig
  (** {1 Irmin stores}

      Irmin stores are tree-like read-write stores with extended capabilities.
      They allow an application (or a collection of applications) to work with
      multiple local states, which can be forked and merged programmatically,
      without having to rely on a global state. In a way very similar to version
      control systems, Irmin local states are called {i branches}.

      There are two kinds of store in Irmin: the ones based on {{!persistent}
      persistent} named branches and the ones based {{!temporary} temporary}
      detached heads. These exist relative to a local, larger (and shared)
      store, and have some (shared) contents. This is exactly the same as usual
      version control systems, that the informed user can see as an implicit
      purely functional data-structure. *)

  type repo
  (** The type for Irmin repositories. *)

  type t
  (** The type for Irmin stores. *)

  type step
  (** The type for {!key} steps. *)

  type key
  (** The type for store keys. A key is a sequence of {!step}s. *)

  type metadata
  (** The type for store metadata. *)

  type contents
  (** The type for store contents. *)

  type node
  (** The type for store nodes. *)

  type tree = [ `Node of node | `Contents of contents * metadata ]
  (** The type for store trees. *)

  type hash
  (** The type for object hashes. *)

  type commit
  (** Type for commit identifiers. Similar to Git's commit SHA1s. *)

  type branch
  (** Type for persistent branch names. Branches usually share a common global
      namespace and it's the user's responsibility to avoid name clashes. *)

  type slice
  (** Type for store slices. *)

  type lca_error = [ `Max_depth_reached | `Too_many_lcas ]
  (** The type for errors associated with functions computing least common
      ancestors *)

  type ff_error = [ `No_change | `Rejected | lca_error ]
  (** The type for errors for {!fast_forward}. *)

  (** Repositories. *)
  module Repo : sig
    (** {1 Repositories}

        A repository contains a set of branches. *)

    type t = repo
    (** The type of repository handles. *)

    val v : config -> t Lwt.t
    (** [v config] connects to a repository in a backend-specific manner. *)

    val close : t -> unit Lwt.t
    (** [close t] frees up all resources associated with [t]. Any operations run
        on a closed repository will raise {!Closed}. *)

    val heads : t -> commit list Lwt.t
    (** [heads] is {!Head.list}. *)

    val branches : t -> branch list Lwt.t
    (** [branches] is {!Branch.list}. *)

    val export :
      ?full:bool ->
      ?depth:int ->
      ?min:commit list ->
      ?max:[ `Head | `Max of commit list ] ->
      t ->
      slice Lwt.t
    (** [export t ~full ~depth ~min ~max] exports the store slice between [min]
        and [max], using at most [depth] history depth (starting from the max).

        If [max] is `Head (also the default value), use the current [heads]. If
        [min] is not specified, use an unbound past (but can still be limited by
        [depth]).

        [depth] is used to limit the depth of the commit history. [None] here
        means no limitation.

        If [full] is set (default is true), the full graph, including the
        commits, nodes and contents, is exported, otherwise it is the commit
        history graph only. *)

    val import : t -> slice -> (unit, [ `Msg of string ]) result Lwt.t
    (** [import t s] imports the contents of the slice [s] in [t]. Does not
        modify branches. *)
  end

  val empty : repo -> t Lwt.t
  (** [empty repo] is a temporary, empty store. Becomes a normal temporary store
      after the first update. *)

  val master : repo -> t Lwt.t
  (** [master repo] is a persistent store based on [r]'s master branch. This
      operation is cheap, can be repeated multiple times. *)

  val of_branch : repo -> branch -> t Lwt.t
  (** [of_branch r name] is a persistent store based on the branch [name].
      Similar to [master], but use [name] instead {!Branch.S.master}. *)

  val of_commit : commit -> t Lwt.t
  (** [of_commit c] is a temporary store, based on the commit [c].

      Temporary stores do not have stable names: instead they can be addressed
      using the hash of the current commit. Temporary stores are similar to
      Git's detached heads. In a temporary store, all the operations are
      performed relative to the current head and update operations can modify
      the current head: the current stores's head will automatically become the
      new head obtained after performing the update. *)

  val repo : t -> repo
  (** [repo t] is the repository containing [t]. *)

  val tree : t -> tree Lwt.t
  (** [tree t] is [t]'s current tree. Contents is not allowed at the root of the
      tree. *)

  module Status : sig
    type t = [ `Empty | `Branch of branch | `Commit of commit ]
    (** The type for store status. *)

    val t : repo -> t Type.t
    (** [t] is the value type for {!t}. *)

    val pp : t Fmt.t
    (** [pp] is the pretty-printer for store status. *)
  end

  val status : t -> Status.t
  (** [status t] is [t]'s status. It can either be a branch, a commit or empty. *)

  (** Managing the store's heads. *)
  module Head : sig
    val list : repo -> commit list Lwt.t
    (** [list t] is the list of all the heads in local store. Similar to
        [git rev-list --all]. *)

    val find : t -> commit option Lwt.t
    (** [find t] is the current head of the store [t]. This works for both
        persistent and temporary branches. In the case of a persistent branch,
        this involves getting the the head associated with the branch, so this
        may block. In the case of a temporary store, it simply returns the
        current head. Returns [None] if the store has no contents. Similar to
        [git rev-parse HEAD]. *)

    val get : t -> commit Lwt.t
    (** Same as {!find} but raise [Invalid_argument] if the store does not have
        any contents. *)

    val set : t -> commit -> unit Lwt.t
    (** [set t h] updates [t]'s contents with the contents of the commit [h].
        Can cause data loss as it discards the current contents. Similar to
        [git reset --hard <hash>]. *)

    val fast_forward :
      t -> ?max_depth:int -> ?n:int -> commit -> (unit, ff_error) result Lwt.t
    (** [fast_forward t h] is similar to {!update} but the [t]'s head is updated
        to [h] only if [h] is stricly in the future of [t]'s current head.
        [max_depth] or [n] are used to limit the search space of the lowest
        common ancestors (see {!lcas}).

        The result is:

        - [Ok ()] if the operation is succesfull;
        - [Error `No_change] if [h] is already [t]'s head;
        - [Error `Rejected] if [h] is not in the strict future of [t]'s head.
        - [Error e] if the history exploration has been cut before getting
          useful results. In that case. the operation can be retried using
          different parameters of [n] and [max_depth] to get better results. *)

    val test_and_set :
      t -> test:commit option -> set:commit option -> bool Lwt.t
    (** Same as {!update_head} but check that the value is [test] before
        updating to [set]. Use {!update} or {!merge} instead if possible. *)

    val merge :
      into:t ->
      info:Info.f ->
      ?max_depth:int ->
      ?n:int ->
      commit ->
      (unit, Merge.conflict) result Lwt.t
    (** [merge ~into:t ?max_head ?n commit] merges the contents of the commit
        associated to [commit] into [t]. [max_depth] is the maximal depth used
        for getting the lowest common ancestor. [n] is the maximum number of
        lowest common ancestors. If present, [max_depth] or [n] are used to
        limit the search space of the lowest common ancestors (see {!lcas}). *)
  end

  module Hash : HASH with type t = hash
  (** Object hashes. *)

  (** [Commit] defines immutable objects to describe store updates. *)
  module Commit : sig
    type t = commit
    (** The type for store commits. *)

    val t : repo -> t Type.t
    (** [t] is the value type for {!t}. *)

    val pp_hash : t Fmt.t
    (** [pp] is the pretty-printer for commit. Display only the hash. *)

    val v : repo -> info:Info.t -> parents:hash list -> tree -> commit Lwt.t
    (** [v r i ~parents:p t] is the commit [c] such that:

        - [info c = i]
        - [parents c = p]
        - [tree c = t] *)

    val tree : commit -> tree
    (** [tree c] is [c]'s root tree. *)

    val parents : commit -> hash list
    (** [parents c] are [c]'s parents. *)

    val info : commit -> Info.t
    (** [info c] is [c]'s info. *)

    (** {1 Import/Export} *)

    val hash : commit -> hash
    (** [hash c] it [c]'s hash. *)

    val of_hash : repo -> hash -> commit option Lwt.t
    (** [of_hash r h] is the the commit object in [r] having [h] as hash, or
        [None] is no such commit object exists. *)
  end

  (** [Contents] provides base functions for the store's contents. *)
  module Contents : sig
    include CONTENTS with type t = contents

    (** {1 Import/Export} *)

    val hash : contents -> hash
    (** [hash c] it [c]'s hash in the repository [r]. *)

    val of_hash : repo -> hash -> contents option Lwt.t
    (** [of_hash r h] is the the contents object in [r] having [h] as hash, or
        [None] is no such contents object exists. *)
  end

  (** Managing store's trees. *)

  module Tree : sig
    include
      TREE
        with type step := step
         and type key := key
         and type metadata := metadata
         and type contents := contents
         and type node := node
         and type tree := tree

    (** {1 Import/Export} *)

    val hash : tree -> hash
    (** [hash r c] it [c]'s hash in the repository [r]. *)

    val of_hash : Repo.t -> hash -> tree option Lwt.t
    (** [of_hash r h] is the the tree object in [r] having [h] as hash, or
        [None] is no such tree object exists. *)

    val shallow : Repo.t -> hash -> tree
    (** [shallow r h] is the shallow tree object with the hash [h]. No check is
        performed to verify if [h] actually exists in [r]. *)
  end

  (** {1 Reads} *)

  val kind : t -> key -> [ `Contents | `Node ] option Lwt.t
  (** [kind] is {!Tree.kind} applied to [t]'s root tree. *)

  val list : t -> key -> (step * [ `Contents | `Node ]) list Lwt.t
  (** [list t] is {!Tree.list} applied to [t]'s root tree. *)

  val mem : t -> key -> bool Lwt.t
  (** [mem t] is {!Tree.mem} applied to [t]'s root tree. *)

  val mem_tree : t -> key -> bool Lwt.t
  (** [mem_tree t] is {!Tree.mem_tree} applied to [t]'s root tree. *)

  val find_all : t -> key -> (contents * metadata) option Lwt.t
  (** [find_all t] is {!Tree.find_all} applied to [t]'s root tree. *)

  val find : t -> key -> contents option Lwt.t
  (** [find t] is {!Tree.find} applied to [t]'s root tree. *)

  val get_all : t -> key -> (contents * metadata) Lwt.t
  (** [get_all t] is {!Tree.get_all} applied on [t]'s root tree. *)

  val get : t -> key -> contents Lwt.t
  (** [get t] is {!Tree.get} applied to [t]'s root tree. *)

  val find_tree : t -> key -> tree option Lwt.t
  (** [find_tree t] is {!Tree.find_tree} applied to [t]'s root tree. *)

  val get_tree : t -> key -> tree Lwt.t
  (** [get_tree t k] is {!Tree.get_tree} applied to [t]'s root tree. *)

  val hash : t -> key -> hash option Lwt.t
  (** [hash t k] *)

  (** {1 Udpates} *)

  type write_error =
    [ Merge.conflict | `Too_many_retries of int | `Test_was of tree option ]
  (** The type for write errors.

      - Merge conflict.
      - Concurrent transactions are competing to get the current operation
        committed and too many attemps have been tried (livelock).
      - A "test and set" operation has failed and the current value is [v]
        instead of the one we were waiting for. *)

  val set :
    ?retries:int ->
    ?allow_empty:bool ->
    ?parents:commit list ->
    info:Info.f ->
    t ->
    key ->
    contents ->
    (unit, write_error) result Lwt.t
  (** [set t k ~info v] sets [k] to the value [v] in [t]. Discard any previous
      results but ensure that no operation is lost in the history.

      This function always uses {!Metadata.default} as metadata. Use {!set_tree}
      with `[Contents (c, m)] for different ones.

      The result is [Error `Too_many_retries] if the concurrent operations do
      not allow the operation to commit to the underlying storage layer
      (livelock). *)

  val set_exn :
    ?retries:int ->
    ?allow_empty:bool ->
    ?parents:commit list ->
    info:Info.f ->
    t ->
    key ->
    contents ->
    unit Lwt.t
  (** [set_exn] is like {!set} but raise [Failure _] instead of using a result
      type. *)

  val set_tree :
    ?retries:int ->
    ?allow_empty:bool ->
    ?parents:commit list ->
    info:Info.f ->
    t ->
    key ->
    tree ->
    (unit, write_error) result Lwt.t
  (** [set_tree] is like {!set} but for trees. *)

  val set_tree_exn :
    ?retries:int ->
    ?allow_empty:bool ->
    ?parents:commit list ->
    info:Info.f ->
    t ->
    key ->
    tree ->
    unit Lwt.t
  (** [set_tree] is like {!set_exn} but for trees. *)

  val remove :
    ?retries:int ->
    ?allow_empty:bool ->
    ?parents:commit list ->
    info:Info.f ->
    t ->
    key ->
    (unit, write_error) result Lwt.t
  (** [remove t ~info k] remove any bindings to [k] in [t].

      The result is [Error `Too_many_retries] if the concurrent operations do
      not allow the operation to commit to the underlying storage layer
      (livelock). *)

  val remove_exn :
    ?retries:int ->
    ?allow_empty:bool ->
    ?parents:commit list ->
    info:Info.f ->
    t ->
    key ->
    unit Lwt.t
  (** [remove_exn] is like {!remove} but raise [Failure _] instead of a using
      result type. *)

  val test_and_set :
    ?retries:int ->
    ?allow_empty:bool ->
    ?parents:commit list ->
    info:Info.f ->
    t ->
    key ->
    test:contents option ->
    set:contents option ->
    (unit, write_error) result Lwt.t
  (** [test_and_set ~test ~set] is like {!set} but it atomically checks that the
      tree is [test] before modifying it to [set].

      This function always uses {!Metadata.default} as metadata. Use
      {!test_and_set_tree} with `[Contents (c, m)] for different ones.

      The result is [Error (`Test t)] if the current tree is [t] instead of
      [test].

      The result is [Error `Too_many_retries] if the concurrent operations do
      not allow the operation to commit to the underlying storage layer
      (livelock). *)

  val test_and_set_exn :
    ?retries:int ->
    ?allow_empty:bool ->
    ?parents:commit list ->
    info:Info.f ->
    t ->
    key ->
    test:contents option ->
    set:contents option ->
    unit Lwt.t
  (** [test_and_set_exn] is like {!test_and_set} but raise [Failure _] instead
      of using a result type. *)

  val test_and_set_tree :
    ?retries:int ->
    ?allow_empty:bool ->
    ?parents:commit list ->
    info:Info.f ->
    t ->
    key ->
    test:tree option ->
    set:tree option ->
    (unit, write_error) result Lwt.t
  (** [test_and_set_tree] is like {!test_and_set} but for trees. *)

  val test_and_set_tree_exn :
    ?retries:int ->
    ?allow_empty:bool ->
    ?parents:commit list ->
    info:Info.f ->
    t ->
    key ->
    test:tree option ->
    set:tree option ->
    unit Lwt.t
  (** [test_and_set_tree_exn] is like {!test_and_set_exn} but for trees. *)

  val merge :
    ?retries:int ->
    ?allow_empty:bool ->
    ?parents:commit list ->
    info:Info.f ->
    old:contents option ->
    t ->
    key ->
    contents option ->
    (unit, write_error) result Lwt.t
  (** [merge ~old] is like {!set} but merge the current tree and the new tree
      using [old] as ancestor in case of conflicts.

      This function always uses {!Metadata.default} as metadata. Use
      {!merge_tree} with `[Contents (c, m)] for different ones.

      The result is [Error (`Conflict c)] if the merge failed with the conflict
      [c].

      The result is [Error `Too_many_retries] if the concurrent operations do
      not allow the operation to commit to the underlying storage layer
      (livelock). *)

  val merge_exn :
    ?retries:int ->
    ?allow_empty:bool ->
    ?parents:commit list ->
    info:Info.f ->
    old:contents option ->
    t ->
    key ->
    contents option ->
    unit Lwt.t
  (** [merge_exn] is like {!merge} but raise [Failure _] instead of using a
      result type. *)

  val merge_tree :
    ?retries:int ->
    ?allow_empty:bool ->
    ?parents:commit list ->
    info:Info.f ->
    old:tree option ->
    t ->
    key ->
    tree option ->
    (unit, write_error) result Lwt.t
  (** [merge_tree] is like {!merge_tree} but for trees. *)

  val merge_tree_exn :
    ?retries:int ->
    ?allow_empty:bool ->
    ?parents:commit list ->
    info:Info.f ->
    old:tree option ->
    t ->
    key ->
    tree option ->
    unit Lwt.t
  (** [merge_tree] is like {!merge_tree} but for trees. *)

  val with_tree :
    ?retries:int ->
    ?allow_empty:bool ->
    ?parents:commit list ->
    ?strategy:[ `Set | `Test_and_set | `Merge ] ->
    info:Info.f ->
    t ->
    key ->
    (tree option -> tree option Lwt.t) ->
    (unit, write_error) result Lwt.t
  (** [with_tree t k ~info f] replaces {i atomically} the subtree [v] under [k]
      in the store [t] by the contents of the tree [f v], using the commit info
      [info ()].

      If [v = f v] and [allow_empty] is unset (default) then, the operation is a
      no-op.

      If [v != f v] and no other changes happen concurrently, [f v] becomes the
      new subtree under [k]. If other changes happen concurrently to that
      operations, the semantics depend on the value of [strategy]:

      - if [strategy = `Set], use {!set} and discard any concurrent updates to
        [k].
      - if [strategy = `Test_and_set] (default), use {!test_and_set} and ensure
        that no concurrent operations are updating [k].
      - if [strategy = `Merge], use {!merge} and ensure that concurrent updates
        and merged with the values present at the beginning of the transaction.

      {b Note:} Irmin transactions provides
      {{:https://en.wikipedia.org/wiki/Snapshot_isolation} snapshot isolation}
      guarantees: reads and writes are isolated in every transaction, but only
      write conflicts are visible on commit. *)

  val with_tree_exn :
    ?retries:int ->
    ?allow_empty:bool ->
    ?parents:commit list ->
    ?strategy:[ `Set | `Test_and_set | `Merge ] ->
    info:Info.f ->
    t ->
    key ->
    (tree option -> tree option Lwt.t) ->
    unit Lwt.t
  (** [with_tree_exn] is like {!with_tree} but raise [Failure _] instead of
      using a return type. *)

  (** {1 Clones} *)

  val clone : src:t -> dst:branch -> t Lwt.t
  (** [clone ~src ~dst] makes [dst] points to [Head.get src]. [dst] is created
      if needed. Remove the current contents en [dst] if [src] is {!empty}. *)

  (** {1 Watches} *)

  type watch
  (** The type for store watches. *)

  val watch : t -> ?init:commit -> (commit diff -> unit Lwt.t) -> watch Lwt.t
  (** [watch t f] calls [f] every time the contents of [t]'s head is updated.

      {b Note:} even if [f] might skip some head updates, it will never be
      called concurrently: all consecutive calls to [f] are done in sequence, so
      we ensure that the previous one ended before calling the next one. *)

  val watch_key :
    t ->
    key ->
    ?init:commit ->
    ((commit * tree) diff -> unit Lwt.t) ->
    watch Lwt.t
  (** [watch_key t key f] calls [f] every time the [key]'s value is added,
      removed or updated. If the current branch is deleted, no signal is sent to
      the watcher. *)

  val unwatch : watch -> unit Lwt.t
  (** [unwatch w] disable [w]. Return once the [w] is fully disabled. *)

  (** {1 Merges and Common Ancestors.} *)

  type 'a merge =
    info:Info.f ->
    ?max_depth:int ->
    ?n:int ->
    'a ->
    (unit, Merge.conflict) result Lwt.t
  (** The type for merge functions. *)

  val merge_into : into:t -> t merge
  (** [merge_into ~into i t] merges [t]'s current branch into [x]'s current
      branch using the info [i]. After that operation, the two stores are still
      independent. Similar to [git merge <branch>]. *)

  val merge_with_branch : t -> branch merge
  (** Same as {!merge} but with a branch ID. *)

  val merge_with_commit : t -> commit merge
  (** Same as {!merge} but with a commit ID. *)

  val lcas :
    ?max_depth:int -> ?n:int -> t -> t -> (commit list, lca_error) result Lwt.t
  (** [lca ?max_depth ?n msg t1 t2] returns the collection of least common
      ancestors between the heads of [t1] and [t2] branches.

      - [max_depth] is the maximum depth of the exploration (default is
        [max_int]). Return [Error `Max_depth_reached] if this depth is exceeded.
      - [n] is the maximum expected number of lcas. Stop the exploration as soon
        as [n] lcas are found. Return [Error `Too_many_lcas] if more [lcas] are
        found. *)

  val lcas_with_branch :
    t ->
    ?max_depth:int ->
    ?n:int ->
    branch ->
    (commit list, lca_error) result Lwt.t
  (** Same as {!lcas} but takes a branch ID as argument. *)

  val lcas_with_commit :
    t ->
    ?max_depth:int ->
    ?n:int ->
    commit ->
    (commit list, lca_error) result Lwt.t
  (** Same as {!lcas} but takes a commit ID as argument. *)

  (** {1 History} *)

  module History : Graph.Sig.P with type V.t = commit
  (** An history is a DAG of heads. *)

  val history :
    ?depth:int -> ?min:commit list -> ?max:commit list -> t -> History.t Lwt.t
  (** [history ?depth ?min ?max t] is a view of the history of the store [t], of
      depth at most [depth], starting from the [t]'s head (or from [max] if the
      head is not set) and stopping at [min] if specified. *)

  val last_modified : ?depth:int -> ?n:int -> t -> key -> commit list Lwt.t
  (** [last_modified ?number c k] is the list of the last [number] commits that
      modified [key], in ascending order of date. [depth] is the maximum depth
      to be explored in the commit graph, if any. Default value for [number] is
      1. *)

  (** Manipulate branches. *)
  module Branch : sig
    (** {1 Branch Store}

        Manipulate relations between {{!branch} branches} and {{!commit}
        commits}. *)

    val mem : repo -> branch -> bool Lwt.t
    (** [mem r b] is true iff [b] is present in [r]. *)

    val find : repo -> branch -> commit option Lwt.t
    (** [find r b] is [Some c] iff [c] is bound to [b] in [t]. It is [None] if
        [b] is not present in [t]. *)

    val get : repo -> branch -> commit Lwt.t
    (** [get t b] is similar to {!find} but raise [Invalid_argument] if [b] is
        not present in [t]. *)

    val set : repo -> branch -> commit -> unit Lwt.t
    (** [set t b c] bounds [c] to [b] in [t]. *)

    val remove : repo -> branch -> unit Lwt.t
    (** [remove t b] removes [b] from [t]. *)

    val list : repo -> branch list Lwt.t
    (** [list t] is the list of branches present in [t]. *)

    val watch :
      repo ->
      branch ->
      ?init:commit ->
      (commit diff -> unit Lwt.t) ->
      watch Lwt.t
    (** [watch t b f] calls [f] on every change in [b]. *)

    val watch_all :
      repo ->
      ?init:(branch * commit) list ->
      (branch -> commit diff -> unit Lwt.t) ->
      watch Lwt.t
    (** [watch_all t f] calls [f] on every branch-related change in [t],
        including creation/deletion events. *)

    include BRANCH with type t = branch
    (** Base functions for branches. *)
  end

  (** [Key] provides base functions for the stores's paths. *)
  module Key : PATH with type t = key and type step = step

  module Metadata : METADATA with type t = metadata
  (** [Metadata] provides base functions for node metadata. *)

  (** {1 Value Types} *)

  val step_t : step Type.t
  (** [step_t] is the value type for {!step}. *)

  val key_t : key Type.t
  (** [key_t] is the value type for {!key}. *)

  val metadata_t : metadata Type.t
  (** [metadata_t] is the value type for {!metadata}. *)

  val contents_t : contents Type.t
  (** [contents_t] is the value type for {!contents}. *)

  val node_t : node Type.t
  (** [node_t] is the value type for {!node}. *)

  val tree_t : tree Type.t
  (** [tree_t] is the value type for {!tree}. *)

  val commit_t : repo -> commit Type.t
  (** [commit_t r] is the value type for {!commit}. *)

  val branch_t : branch Type.t
  (** [branch_t] is the value type for {!branch}. *)

  val slice_t : slice Type.t
  (** [slice_t] is the value type for {!slice}. *)

  val kind_t : [ `Contents | `Node ] Type.t
  (** [kind_t] is the value type for values returned by {!kind}. *)

  val lca_error_t : lca_error Type.t
  (** [lca_error_t] is the value type for {!lca_error}. *)

  val ff_error_t : ff_error Type.t
  (** [ff_error_t] is the value type for {!ff_error}. *)

  val write_error_t : write_error Type.t
  (** [write_error_t] is the value type for {!write_error}. *)

  (** Private functions, which might be used by the backends. *)
  module Private : sig
    include
      PRIVATE
        with type Contents.value = contents
         and module Hash = Hash
         and module Node.Path = Key
         and type Node.Metadata.t = metadata
         and type Branch.key = branch
         and type Slice.t = slice
         and type Repo.t = repo
  end

  type remote +=
    | E of Private.Sync.endpoint
          (** Extend the [remote] type with [endpoint]. *)

  (** {2 Converters to private types} *)

  val to_private_node : node -> Private.Node.value option Lwt.t

  val of_private_node : repo -> Private.Node.value -> node

  val to_private_commit : commit -> Private.Commit.value
  (** [to_private_commit c] is the private commit object associated with the
      commit [c]. *)

  val of_private_commit : repo -> Private.Commit.value -> commit
  (** [of_private_commit r c] is the commit associated with the private commit
      object [c]. *)

  val save_contents : [> `Write ] Private.Contents.t -> contents -> hash Lwt.t
  (** Save a content into the database *)

  val save_tree :
    ?clear:bool ->
    repo ->
    [> `Write ] Private.Contents.t ->
    [ `Read | `Write ] Private.Node.t ->
    tree ->
    hash Lwt.t
  (** Save a tree into the database. Does not do any reads. If [clear] is set
      (it is by default), the tree cache will be cleared after the save. *)
end

module type MAKER = functor
  (M : METADATA)
  (C : CONTENTS)
  (P : PATH)
  (B : BRANCH)
  (H : HASH)
  ->
  STORE
    with type key = P.t
     and type step = P.step
     and type metadata = M.t
     and type contents = C.t
     and type branch = B.t
     and type hash = H.t

type remote += Store : (module STORE with type t = 'a) * 'a -> remote

module type SYNC_STORE = sig
  (** {1 Native Synchronization} *)

  type db
  (** Type type for store handles. *)

  type commit
  (** The type for store heads. *)

  type status = [ `Empty | `Head of commit ]
  (** The type for remote status. *)

  val status_t : db -> status Type.t
  (** [status_t db] is the value type for {!status} of remote [db]. *)

  val pp_status : status Fmt.t
  (** [pp_status] pretty-prints return statuses. *)

  val fetch :
    db -> ?depth:int -> remote -> (status, [ `Msg of string ]) result Lwt.t
  (** [fetch t ?depth r] populate the local store [t] with objects for the
      remote store [r], using [t]'s current branch. The [depth] parameter limits
      the history depth. Return [`Empty] if either the local or remote store do
      not have a valid head. *)

  val fetch_exn : db -> ?depth:int -> remote -> status Lwt.t
  (** Same as {!fetch} but raise [Invalid_argument] if either the local or
      remote store do not have a valid head. *)

  type pull_error = [ `Msg of string | Merge.conflict ]
  (** The type for pull errors. *)

  val pp_pull_error : pull_error Fmt.t
  (** [pp_push_error] pretty-prints pull errors. *)

  val pull :
    db ->
    ?depth:int ->
    remote ->
    [ `Merge of Info.f | `Set ] ->
    (status, pull_error) result Lwt.t
  (** [pull t ?depth r s] is similar to {{!Sync.fetch} fetch} but it also
      updates [t]'s current branch. [s] is the update strategy:

      - [`Merge] uses [Head.merge]. Can return a conflict.
      - [`Set] uses [S.Head.set]. *)

  val pull_exn :
    db -> ?depth:int -> remote -> [ `Merge of Info.f | `Set ] -> status Lwt.t
  (** Same as {!pull} but raise [Invalid_arg] in case of conflict. *)

  type push_error = [ `Msg of string | `Detached_head ]
  (** The type for push errors. *)

  val pp_push_error : push_error Fmt.t
  (** [pp_push_error] pretty-prints push errors. *)

  val push : db -> ?depth:int -> remote -> (status, push_error) result Lwt.t
  (** [push t ?depth r] populates the remote store [r] with objects from the
      current store [t], using [t]'s current branch. If [b] is [t]'s current
      branch, [push] also updates the head of [b] in [r] to be the same as in
      [t].

      {b Note:} {e Git} semantics is to update [b] only if the new head if more
      recent. This is not the case in {e Irmin}. *)

  val push_exn : db -> ?depth:int -> remote -> status Lwt.t
  (** Same as {!push} but raise [Invalid_argument] if an error happens. *)
end
