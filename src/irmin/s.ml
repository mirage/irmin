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

  (** The type for path values. *)
  type t

  (** Type type for path's steps. *)
  type step

  (** The empty path. *)
  val empty : t

  (** Create a path from a list of steps. *)
  val v : step list -> t

  (** Check if the path is empty. *)
  val is_empty : t -> bool

  (** Prepend a step to the path. *)
  val cons : step -> t -> t

  (** Append a step to the path. *)
  val rcons : t -> step -> t

  (** Deconstruct the first element of the path. Return [None] if the path is
      empty. *)
  val decons : t -> (step * t) option

  (** Deconstruct the last element of the path. Return [None] if the path is
      empty. *)
  val rdecons : t -> (t * step) option

  (** [map t f] maps [f] over all steps of [t]. *)
  val map : t -> (step -> 'a) -> 'a list

  (** {1 Value Types} *)

  (** [t] is the value type for {!t}. *)
  val t : t Type.t

  (** [step_t] is the value type for {!step}. *)
  val step_t : step Type.t
end

module type HASH = sig
  (** Signature for digest hashes, inspired by Digestif. *)

  (** The type for digest hashes. *)
  type t

  (** Compute a deterministic store key from a sequence of strings. *)
  val hash : ((string -> unit) -> unit) -> t

  (** [short_hash h] is a small hash of [h], to be used for instance as the
      `hash` function of an OCaml [Hashtbl]. *)
  val short_hash : t -> int

  (** [hash_size] is the size of hash results, in bytes. *)
  val hash_size : int

  (** {1 Value Types} *)

  (** [t] is the value type for {!t}. *)
  val t : t Type.t
end

module type TYPED_HASH = sig
  type t

  type value

  (** Compute a deterministic store key from a string. *)
  val hash : value -> t

  (** [short_hash h] is a small hash of [h], to be used for instance as the
      `hash` function of an OCaml [Hashtbl]. *)
  val short_hash : t -> int

  (** [hash_size] is the size of hash results, in bytes. *)
  val hash_size : int

  (** {1 Value Types} *)

  (** [t] is the value type for {!t}. *)
  val t : t Type.t
end

module type CONTENTS = sig
  (** {1 Signature for store contents} *)

  (** The type for user-defined contents. *)
  type t

  (** [t] is the value type for {!t}. *)
  val t : t Type.t

  (** Merge function. Evaluates to [`Conflict msg] if the values cannot be
      merged properly. The arguments of the merge function can take [None] to
      mean that the key does not exists for either the least-common ancestor or
      one of the two merging points. The merge function returns [None] when the
      key's value should be deleted. *)
  val merge : t option Merge.t
end

module type CONTENT_ADDRESSABLE_STORE = sig
  (** {1 Content-addressable stores}

      Content-addressable stores are store where it is possible to read and add
      new values. Keys are derived from the values raw contents and hence are
      deterministic. *)

  (** The type for content-addressable backend stores. The ['a] phantom type
      carries information about the store mutability. *)
  type 'a t

  (** The type for keys. *)
  type key

  (** The type for raw values. *)
  type value

  (** [mem t k] is true iff [k] is present in [t]. *)
  val mem : [> `Read ] t -> key -> bool Lwt.t

  (** [find t k] is [Some v] if [k] is associated to [v] in [t] and [None] is
      [k] is not present in [t]. *)
  val find : [> `Read ] t -> key -> value option Lwt.t

  (** Write the contents of a value to the store. It's the responsibility of the
      content-addressable store to generate a consistent key. *)
  val add : [> `Write ] t -> value -> key Lwt.t

  (** Same as {!add} but allows to specify the key directly. The backend might
      choose to discared that key and/or can be corrupt if the key scheme is not
      consistent. *)
  val unsafe_add : [> `Write ] t -> key -> value -> unit Lwt.t
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

  (** The type for append-only backend stores. The ['a] phantom type carries
      information about the store mutability. *)
  type 'a t

  (** The type for keys. *)
  type key

  (** The type for raw values. *)
  type value

  (** [mem t k] is true iff [k] is present in [t]. *)
  val mem : [> `Read ] t -> key -> bool Lwt.t

  (** [find t k] is [Some v] if [k] is associated to [v] in [t] and [None] is
      [k] is not present in [t]. *)
  val find : [> `Read ] t -> key -> value option Lwt.t

  (** Write the contents of a value to the store. *)
  val add : [> `Write ] t -> key -> value -> unit Lwt.t
end

module type APPEND_ONLY_STORE_MAKER = functor (K : Type.S) (V : Type.S) -> sig
  include APPEND_ONLY_STORE with type key = K.t and type value = V.t

  val batch : [ `Read ] t -> ([ `Read | `Write ] t -> 'a Lwt.t) -> 'a Lwt.t

  val v : Conf.t -> [ `Read ] t Lwt.t

  val close : 'a t -> unit Lwt.t
end

module type METADATA = sig
  (** The type for metadata. *)
  type t

  (** [t] is the value type for {!t}. *)
  val t : t Type.t

  (** [merge] is the merge function for metadata. *)
  val merge : t Merge.t

  (** The default metadata to attach, for APIs that don't care about metadata. *)
  val default : t
end

module type CONTENTS_STORE = sig
  include CONTENT_ADDRESSABLE_STORE

  (** [merge t] lifts the merge functions defined on contents values to contents
      key. The merge function will: {e (i)} read the values associated with the
      given keys, {e (ii)} use the merge function defined on values and {e
      (iii)} write the resulting values into the store to get the resulting key.
      See {!Contents.S.merge}.

      If any of these operations fail, return [`Conflict]. *)
  val merge : [ `Read | `Write ] t -> key option Merge.t

  (** [Key] provides base functions for user-defined contents keys. *)
  module Key : TYPED_HASH with type t = key and type value = value

  (** [Val] provides base functions for user-defined contents values. *)
  module Val : CONTENTS with type t = value
end

module type NODE = sig
  (** {1 Node values} *)

  (** The type for node values. *)
  type t

  (** The type for node metadata. *)
  type metadata

  (** The type for keys. *)
  type hash

  (** The type for steps between nodes. *)
  type step

  (** The type for either (node) keys or (contents) keys combined with their
      metadata. *)
  type value = [ `Node of hash | `Contents of hash * metadata ]

  (** [create l] is a new node. *)
  val v : (step * value) list -> t

  (** [list t] is the contents of [t]. *)
  val list : t -> (step * value) list

  (** [empty] is the empty node. *)
  val empty : t

  (** [is_empty t] is true iff [t] is {!empty}. *)
  val is_empty : t -> bool

  (** [find t s] is the value associated with [s] in [t].

      A node can point to user-defined {{!Node.S.contents} contents}. The edge
      between the node and the contents is labeled by a {{!Node.S.step} step}. *)
  val find : t -> step -> value option

  (** [add t s v] is the node where [find t v] is [Some s] but is similar to [t]
      otherwise. *)
  val add : t -> step -> value -> t

  (** [remove t s] is the node where [find t s] is [None] but is similar to [t]
      otherwise. *)
  val remove : t -> step -> t

  (** {1 Value types} *)

  (** [t] is the value type for {!t}. *)
  val t : t Type.t

  (** [default] is the default metadata value. *)
  val default : metadata

  (** [metadata_t] is the value type for {!metadata}. *)
  val metadata_t : metadata Type.t

  (** [hash_t] is the value type for {!hash}. *)
  val hash_t : hash Type.t

  (** [step_t] is the value type for {!step}. *)
  val step_t : step Type.t

  (** [value_t] is the value type for {!value}. *)
  val value_t : value Type.t
end

module type NODE_GRAPH = sig
  (** {1 Node Graphs} *)

  (** The type for store handles. *)
  type 'a t

  (** The type for node metadata. *)
  type metadata

  (** The type of user-defined contents. *)
  type contents

  (** The type for node values. *)
  type node

  (** The type of steps. A step is used to pass from one node to another. *)
  type step

  (** The type of store paths. A path is composed of {{!step} steps}. *)
  type path

  (** The type for store values. *)
  type value = [ `Node of node | `Contents of contents * metadata ]

  (** The empty node. *)
  val empty : [> `Write ] t -> node Lwt.t

  (** [v t n] is a new node containing [n]. *)
  val v : [> `Write ] t -> (step * value) list -> node Lwt.t

  (** [list t n] is the contents of the node [n]. *)
  val list : [> `Read ] t -> node -> (step * value) list Lwt.t

  (** [find t n p] is the contents of the path [p] starting form [n]. *)
  val find : [> `Read ] t -> node -> path -> value option Lwt.t

  (** [add t n p v] is the node [x] such that [find t x p] is [Some v] and it
      behaves the same [n] for other operations. *)
  val add : [ `Read | `Write ] t -> node -> path -> value -> node Lwt.t

  (** [remove t n path] is the node [x] such that [find t x] is [None] and it
      behhaves then same as [n] for other operations. *)
  val remove : [ `Read | `Write ] t -> node -> path -> node Lwt.t

  (** [closure t min max] is the unordered list of nodes [n] reachable from a
      node of [max] along a path which: (i) either contains no [min] or (ii) it
      ends with a [min].

      {b Note:} Both [min] and [max] are subsets of [n]. *)
  val closure :
    [> `Read ] t -> min:node list -> max:node list -> node list Lwt.t

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

  (** {1 Value Types} *)

  (** [metadat_t] is the value type for {!metadata}. *)
  val metadata_t : metadata Type.t

  (** [contents_t] is the value type for {!contents}. *)
  val contents_t : contents Type.t

  (** [node_t] is the value type for {!node}. *)
  val node_t : node Type.t

  (** [step_t] is the value type for {!step}. *)
  val step_t : step Type.t

  (** [path_t] is the value type for {!path}. *)
  val path_t : path Type.t

  (** [value_t] is the value type for {!value}. *)
  val value_t : value Type.t
end

module type NODE_STORE = sig
  include CONTENT_ADDRESSABLE_STORE

  (** [Path] provides base functions on node paths. *)
  module Path : PATH

  (** [merge] is the 3-way merge function for nodes keys. *)
  val merge : [ `Read | `Write ] t -> key option Merge.t

  (** [Key] provides base functions for node keys. *)
  module Key : TYPED_HASH with type t = key and type value = value

  (** [Metadata] provides base functions for node metadata. *)
  module Metadata : METADATA

  (** [Val] provides base functions for node values. *)
  module Val :
    NODE
      with type t = value
       and type hash = key
       and type metadata = Metadata.t
       and type step = Path.step

  (** [Contents] is the underlying contents store. *)
  module Contents : CONTENTS_STORE with type key = Val.hash
end

type config = Conf.t

type 'a diff = 'a Diff.t

module type COMMIT = sig
  (** {1 Commit values} *)

  (** The type for commit values. *)
  type t

  (** Type for keys. *)
  type hash

  (** Create a commit. *)
  val v : info:Info.t -> node:hash -> parents:hash list -> t

  (** The underlying node. *)
  val node : t -> hash

  (** The commit parents. *)
  val parents : t -> hash list

  (** The commit info. *)
  val info : t -> Info.t

  (** {1 Value Types} *)

  (** [t] is the value type for {!t}. *)
  val t : t Type.t

  (** [hash_t] is the value type for {!hash}. *)
  val hash_t : hash Type.t
end

module type COMMIT_STORE = sig
  (** {1 Commit Store} *)

  include CONTENT_ADDRESSABLE_STORE

  (** [merge] is the 3-way merge function for commit keys. *)
  val merge : [ `Read | `Write ] t -> info:Info.f -> key option Merge.t

  (** [Key] provides base functions for commit keys. *)
  module Key : TYPED_HASH with type t = key and type value = value

  (** [Val] provides functions for commit values. *)
  module Val : COMMIT with type t = value and type hash = key

  (** [Node] is the underlying node store. *)
  module Node : NODE_STORE with type key = Val.hash
end

module type COMMIT_HISTORY = sig
  (** {1 Commit History} *)

  (** The type for store handles. *)
  type 'a t

  (** The type for node values. *)
  type node

  (** The type for commit values. *)
  type commit

  (** The type for commit objects. *)
  type v

  (** Create a new commit. *)
  val v :
    [> `Write ] t ->
    node:node ->
    parents:commit list ->
    info:Info.t ->
    (commit * v) Lwt.t

  (** Get the commit parents.

      Commits form a append-only, fully functional, partial-order
      data-structure: every commit carries the list of its immediate
      predecessors. *)
  val parents : [> `Read ] t -> commit -> commit list Lwt.t

  (** [merge t] is the 3-way merge function for commit. *)
  val merge : [ `Read | `Write ] t -> info:Info.f -> commit Merge.t

  (** Find the lowest common ancestors
      {{:http://en.wikipedia.org/wiki/Lowest_common_ancestor} lca} between two
      commits. *)
  val lcas :
    [> `Read ] t ->
    ?max_depth:int ->
    ?n:int ->
    commit ->
    commit ->
    (commit list, [ `Max_depth_reached | `Too_many_lcas ]) result Lwt.t

  (** Compute the lowest common ancestors ancestor of a list of commits by
      recursively calling {!lcas} and merging the results.

      If one of the merges results in a conflict, or if a call to {!lcas}
      returns either [Error `Max_depth_reached] or [Error `Too_many_lcas] then
      the function returns the same error. *)
  val lca :
    [ `Read | `Write ] t ->
    info:Info.f ->
    ?max_depth:int ->
    ?n:int ->
    commit list ->
    (commit option, Merge.conflict) result Lwt.t

  (** Compute the {!lcas} of the two commit and 3-way merge the result. *)
  val three_way_merge :
    [ `Read | `Write ] t ->
    info:Info.f ->
    ?max_depth:int ->
    ?n:int ->
    commit ->
    commit ->
    (commit, Merge.conflict) result Lwt.t

  (** Same as {{!Private.Node.GRAPH.closure} GRAPH.closure} but for the history
      graph. *)
  val closure :
    [> `Read ] t -> min:commit list -> max:commit list -> commit list Lwt.t

  (** {1 Value Types} *)

  (** [commit_t] is the value type for {!commit}. *)
  val commit_t : commit Type.t
end

module type SLICE = sig
  (** {1 Slices} *)

  (** The type for slices. *)
  type t

  (** The type for exported contents. *)
  type contents

  (** The type for exported nodes. *)
  type node

  (** The type for exported commits. *)
  type commit

  (** The type for exported values. *)
  type value = [ `Contents of contents | `Node of node | `Commit of commit ]

  (** Create a new empty slice. *)
  val empty : unit -> t Lwt.t

  (** [add t v] adds [v] to [t]. *)
  val add : t -> value -> unit Lwt.t

  (** [iter t f] calls [f] on all values of [t]. *)
  val iter : t -> (value -> unit Lwt.t) -> unit Lwt.t

  (** {1 Value Types} *)

  (** [t] is the value type for {!t}. *)
  val t : t Type.t

  (** [content_t] is the value type for {!contents}. *)
  val contents_t : contents Type.t

  (** [node_t] is the value type for {!node}. *)
  val node_t : node Type.t

  (** [commit_t] is the value type for {!commit}. *)
  val commit_t : commit Type.t

  (** [value_t] is the value type for {!value}. *)
  val value_t : value Type.t
end

module type BRANCH = sig
  (** {1 Signature for Branches} *)

  (** The type for branches. *)
  type t

  (** [t] is the value type for {!t}. *)
  val t : t Type.t

  (** The name of the master branch. *)
  val master : t

  (** Check if the branch is valid. *)
  val is_valid : t -> bool
end

module type ATOMIC_WRITE_STORE = sig
  (** {1 Atomic write stores}

      Atomic-write stores are stores where it is possible to read, update and
      remove elements, with atomically guarantees. *)

  (** The type for atomic-write backend stores. *)
  type t

  (** The type for keys. *)
  type key

  (** The type for raw values. *)
  type value

  (** [mem t k] is true iff [k] is present in [t]. *)
  val mem : t -> key -> bool Lwt.t

  (** [find t k] is [Some v] if [k] is associated to [v] in [t] and [None] is
      [k] is not present in [t]. *)
  val find : t -> key -> value option Lwt.t

  (** [set t k v] replaces the contents of [k] by [v] in [t]. If [k] is not
      already defined in [t], create a fresh binding. Raise [Invalid_argument]
      if [k] is the {{!Path.empty} empty path}. *)
  val set : t -> key -> value -> unit Lwt.t

  (** [test_and_set t key ~test ~set] sets [key] to [set] only if the current
      value of [key] is [test] and in that case returns [true]. If the current
      value of [key] is different, it returns [false]. [None] means that the
      value does not have to exist or is removed.

      {b Note:} The operation is guaranteed to be atomic. *)
  val test_and_set :
    t -> key -> test:value option -> set:value option -> bool Lwt.t

  (** [remove t k] remove the key [k] in [t]. *)
  val remove : t -> key -> unit Lwt.t

  (** [list t] it the list of keys in [t]. *)
  val list : t -> key list Lwt.t

  (** The type of watch handlers. *)
  type watch

  (** [watch t ?init f] adds [f] to the list of [t]'s watch handlers and returns
      the watch handler to be used with {!unwatch}. [init] is the optional
      initial values. It is more efficient to use {!watch_key} to watch only a
      single given key.*)
  val watch :
    t ->
    ?init:(key * value) list ->
    (key -> value diff -> unit Lwt.t) ->
    watch Lwt.t

  (** [watch_key t k ?init f] adds [f] to the list of [t]'s watch handlers for
      the key [k] and returns the watch handler to be used with {!unwatch}.
      [init] is the optional initial value of the key. *)
  val watch_key :
    t -> key -> ?init:value -> (value diff -> unit Lwt.t) -> watch Lwt.t

  (** [unwatch t w] removes [w] from [t]'s watch handlers. *)
  val unwatch : t -> watch -> unit Lwt.t

  (** [close t] frees up all the resources associated to [t]. Any operations run
      on a closed store will raise {!Closed}. *)
  val close : t -> unit Lwt.t
end

module type ATOMIC_WRITE_STORE_MAKER = functor (K : Type.S) (V : Type.S) -> sig
  include ATOMIC_WRITE_STORE with type key = K.t and type value = V.t

  val v : Conf.t -> t Lwt.t
end

module type BRANCH_STORE = sig
  (** {1 Branch Store} *)

  include ATOMIC_WRITE_STORE

  (** Base functions on keys. *)
  module Key : BRANCH with type t = key

  (** Base functions on values. *)
  module Val : HASH with type t = value
end

type remote = ..

module type SYNC = sig
  (** {1 Remote synchronization} *)

  (** The type for store handles. *)
  type t

  (** The type for store heads. *)
  type commit

  (** The type for branch IDs. *)
  type branch

  (** The type for sync endpoints. *)
  type endpoint

  (** [fetch t uri] fetches the contents of the remote store located at [uri]
      into the local store [t]. Return the head of the remote branch with the
      same name, which is now in the local store. [No_head] means no such branch
      exists. *)
  val fetch :
    t ->
    ?depth:int ->
    endpoint ->
    branch ->
    (commit option, [ `Msg of string ]) result Lwt.t

  (** [push t uri] pushes the contents of the local store [t] into the remote
      store located at [uri]. *)
  val push :
    t ->
    ?depth:int ->
    endpoint ->
    branch ->
    (unit, [ `Msg of string | `Detached_head ]) result Lwt.t
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

  (** [empty] is the empty tree. The empty tree does not have associated backend
      configuration values, as they can perform in-memory operation,
      independently of any given backend. *)
  val empty : tree

  (** [of_contents c] is the subtree built from the contents [c]. *)
  val of_contents : ?metadata:metadata -> contents -> tree

  (** [of_node n] is the subtree built from the node [n]. *)
  val of_node : node -> tree

  (** [kind t k] is the type of [s] in [t]. It could either be a tree node or
      some file contents. It is [None] if [k] is not present in [t]. *)
  val kind : tree -> key -> [ `Contents | `Node ] option Lwt.t

  (** [list t key] is the list of files and sub-nodes stored under [k] in [t]. *)
  val list : tree -> key -> (step * [ `Contents | `Node ]) list Lwt.t

  (** {1 Diffs} *)

  (** [diff x y] is the difference of contents between [x] and [y]. *)
  val diff : tree -> tree -> (key * (contents * metadata) diff) list Lwt.t

  (** {1 Manipulating Contents} *)

  (** [mem t k] is true iff [k] is associated to some contents in [t]. *)
  val mem : tree -> key -> bool Lwt.t

  (** [find_all t k] is [Some (b, m)] if [k] is associated to the contents [b]
      and metadata [m] in [t] and [None] if [k] is not present in [t]. *)
  val find_all : tree -> key -> (contents * metadata) option Lwt.t

  (** [find] is similar to {!find_all} but it discards metadata. *)
  val find : tree -> key -> contents option Lwt.t

  (** Same as {!find_all} but raise [Invalid_arg] if [k] is not present in [t]. *)
  val get_all : tree -> key -> (contents * metadata) Lwt.t

  (** Same as {!get_all} but ignore the metadata. *)
  val get : tree -> key -> contents Lwt.t

  (** [add t k c] is the tree where the key [k] is bound to the contents [c] but
      is similar to [t] for other bindings. *)
  val add : tree -> key -> ?metadata:metadata -> contents -> tree Lwt.t

  (** [remove t k] is the tree where [k] bindings has been removed but is
      similar to [t] for other bindings. *)
  val remove : tree -> key -> tree Lwt.t

  (** {1 Manipulating Subtrees} *)

  (** [mem_tree t k] is false iff [find_tree k = None]. *)
  val mem_tree : tree -> key -> bool Lwt.t

  (** [find_tree t k] is [Some v] if [k] is associated to [v] in [t]. It is
      [None] if [k] is not present in [t]. *)
  val find_tree : tree -> key -> tree option Lwt.t

  (** [get_tree t k] is [v] if [k] is associated to [v] in [t]. Raise
      [Invalid_arg] if [k] is not present in [t].*)
  val get_tree : tree -> key -> tree Lwt.t

  (** [add_tree t k v] is the tree where the key [k] is bound to the tree [v]
      but is similar to [t] for other bindings *)
  val add_tree : tree -> key -> tree -> tree Lwt.t

  (** [merge] is the 3-way merge function for trees. *)
  val merge : tree Merge.t

  (** {1 Folds} *)

  (** The type for fold marks. *)
  type marks

  (** [empty_marks ()] is an empty collection of marks. *)
  val empty_marks : unit -> marks

  (** The type for {!fold}'s [force] parameter. [`True] forces the fold to read
      the objects of the lazy nodes. [`False f] is applying [f] on every lazy
      node instead. *)
  type 'a force = [ `True | `False of key -> 'a -> 'a Lwt.t ]

  (** The type for {!fold}'s [uniq] parameters. [`False] folds over all the
      nodes. [`True] does not recurse on nodes already seen. [`Marks m] uses the
      collection of marks [m] to store the cache of keys: the fold will modify
      [m]. This can be used for incremental folds. *)
  type uniq = [ `False | `True | `Marks of marks ]

  (** The type for {!fold}'s [pre] and [post] parameters. *)
  type 'a node_fn = key -> step list -> 'a -> 'a Lwt.t

  (** [fold f t acc] folds [f] over [t]'s leafs.

      For every node [n], ui [n] is a leaf node, call [f path n]. Otherwise:

      - Call [pre path n]. By default [pre] is the identity;
      - Recursively call [fold] on each children, in lexicographic order;
      - Call [post path n]; By default [post] is the identity.

      See {!force} for details about the [force] parameters. By default it is
      [`True].

      See {!uniq} for details about the [uniq] parameters. By default it is
      [`False]. *)
  val fold :
    ?force:'a force ->
    ?uniq:uniq ->
    ?pre:'a node_fn ->
    ?post:'a node_fn ->
    (key -> contents -> 'a -> 'a Lwt.t) ->
    tree ->
    'a ->
    'a Lwt.t

  (** {1 Stats} *)

  (** The type for tree stats. *)
  type stats = {
    nodes : int;  (** Number of node. *)
    leafs : int;  (** Number of leafs. *)
    skips : int;  (** Number of lazy nodes. *)
    depth : int;  (** Maximal depth. *)
    width : int;  (** Maximal width. *)
  }

  (** [pp_stats] is the pretty printer for tree statistics. *)
  val pp_stats : stats Fmt.t

  (** [stats ~force t] are [t]'s statistics. If [force] is true, this will force
      the reading of lazy nodes. By default it is [false]. *)
  val stats : ?force:bool -> tree -> stats Lwt.t

  (** {1 Concrete Trees} *)

  (** The type for concrete trees. *)
  type concrete =
    [ `Tree of (step * concrete) list | `Contents of contents * metadata ]

  (** [of_concrete c] is the subtree equivalent to the concrete tree [c]. *)
  val of_concrete : concrete -> tree

  (** [to_concrete t] is the concrete tree equivalent to the subtree [t]. *)
  val to_concrete : tree -> concrete Lwt.t

  (** {1 Caches} *)

  (** [clear ?depth t] clears all the cache in the tree [t] for subtrees with a
      depth higher than [depth]. If [depth] is not set, all the subtrees are
      cleared. *)
  val clear : ?depth:int -> tree -> unit

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

module type SYNC_STORE = sig
  (** {1 Native Synchronization} *)

  (** Type type for store handles. *)
  type db

  (** The type for store heads. *)
  type commit

  (** The type for remote status. *)
  type status = [ `Empty | `Head of commit ]

  (** [status_t db] is the value type for {!status} of remote [db]. *)
  val status_t : db -> status Type.t

  (** [pp_status] pretty-prints return statuses. *)
  val pp_status : status Fmt.t

  (** [fetch t ?depth r] populate the local store [t] with objects for the
      remote store [r], using [t]'s current branch. The [depth] parameter limits
      the history depth. Return [`Empty] if either the local or remote store do
      not have a valid head. *)
  val fetch :
    db -> ?depth:int -> remote -> (status, [ `Msg of string ]) result Lwt.t

  (** Same as {!fetch} but raise [Invalid_argument] if either the local or
      remote store do not have a valid head. *)
  val fetch_exn : db -> ?depth:int -> remote -> status Lwt.t

  (** The type for pull errors. *)
  type pull_error = [ `Msg of string | Merge.conflict ]

  (** [pp_push_error] pretty-prints pull errors. *)
  val pp_pull_error : pull_error Fmt.t

  (** [pull t ?depth r s] is similar to {{!Sync.fetch} fetch} but it also
      updates [t]'s current branch. [s] is the update strategy:

      - [`Merge] uses [Head.merge]. Can return a conflict.
      - [`Set] uses [S.Head.set]. *)
  val pull :
    db ->
    ?depth:int ->
    remote ->
    [ `Merge of Info.f | `Set ] ->
    (status, pull_error) result Lwt.t

  (** Same as {!pull} but raise [Invalid_arg] in case of conflict. *)
  val pull_exn :
    db -> ?depth:int -> remote -> [ `Merge of Info.f | `Set ] -> status Lwt.t

  (** The type for push errors. *)
  type push_error = [ `Msg of string | `Detached_head ]

  (** [pp_push_error] pretty-prints push errors. *)
  val pp_push_error : push_error Fmt.t

  (** [push t ?depth r] populates the remote store [r] with objects from the
      current store [t], using [t]'s current branch. If [b] is [t]'s current
      branch, [push] also updates the head of [b] in [r] to be the same as in
      [t].

      {b Note:} {e Git} semantics is to update [b] only if the new head if more
      recent. This is not the case in {e Irmin}. *)
  val push : db -> ?depth:int -> remote -> (status, push_error) result Lwt.t

  (** Same as {!push} but raise [Invalid_argument] if an error happens. *)
  val push_exn : db -> ?depth:int -> remote -> status Lwt.t
end
