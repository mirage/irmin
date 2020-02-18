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

(** Irmin public API.

    [Irmin] is a library to design and use persistent stores with built-in
    snapshot, branching and reverting mechanisms. Irmin uses concepts similar to
    {{:http://git-scm.com/} Git} but it exposes them as a high level library
    instead of a complex command-line frontend. It features a {e bidirectional}
    Git backend, where an application can read and persist its state using the
    Git format, fully-compatible with the usual Git tools and workflows.

    Irmin is designed to use a large variety of backends. It is written in pure
    OCaml and does not depend on external C stubs; it is thus very portable and
    aims to run everywhere, from Linux to browser and MirageOS unikernels.

    Consult the {!basics} and {!examples} of use for a quick start. See also the
    {{!Irmin_unix} documentation} for the unix backends.

    {e Release %%VERSION%% - %%HOMEPAGE%%} *)

val version : string
(** The version of the library. *)

(** {1 Preliminaries} *)

module Type = Type
(** Dynamic types for Irmin values. *)

module Info = Info
(** Commit info are used to keep track of the origin of write operations in the
    stores. [Info] models the metadata associated with commit objects in Git. *)

module Merge = Merge
(** [Merge] provides functions to build custom 3-way merge operators for various
    user-defined contents. *)

module Diff = Diff
(** Differences between values. *)

type 'a diff = 'a Diff.t
(** The type for representing differences betwen values. *)

(** {1 Low-level Stores} *)

(** An Irmin store is automatically built from a number of lower-level stores,
    each implementing fewer operations, such as {{!CONTENT_ADDRESSABLE_STORE}
    content-addressable} and {{!ATOMIC_WRITE_STORE} atomic-write} stores. These
    low-level stores are provided by various backends. *)

module type CONTENT_ADDRESSABLE_STORE = S.CONTENT_ADDRESSABLE_STORE
(** Content-addressable backend store. *)

module type APPEND_ONLY_STORE = S.APPEND_ONLY_STORE
(** Append-only backend store. *)

module type ATOMIC_WRITE_STORE = S.ATOMIC_WRITE_STORE
(** Atomic-write stores. *)

(** {1 User-Defined Contents} *)

(** Store paths.

    An Irmin {{!Irmin.S} store} binds {{!Path.S.t} paths} to user-defined
    {{!Contents.S} contents}. Paths are composed by basic elements, that we call
    {{!Path.S.step} steps}. The following [Path] module provides functions to
    manipulate steps and paths. *)
module Path : sig
  (** {1 Path} *)

  module type S = S.PATH
  (** Signature for path implementations.*)

  (** An implementation of paths as string lists. *)
  module String_list : S with type step = string and type t = string list
end

(** Hashing functions.

    [Hash] provides user-defined hash functions to digest serialized contents.
    Some {{!backend} backends} might be parameterized by such hash functions,
    others might work with a fixed one (for instance, the Git format uses only
    {{!Hash.SHA1} SHA1}).

    A {{!Hash.SHA1} SHA1} implementation is available to pass to the backends. *)
module Hash : sig
  (** {1 Contents Hashing} *)

  module type S = S.HASH
  (** Signature for hash values. *)

  module type TYPED = S.TYPED_HASH
  (** Signature for typed hashes, where [hash] directly takes a value as
      argument and incremental hashing is not possible. *)

  include module type of Hash
end

(** [Metadata] defines metadata that is attached to contents but stored in
    nodes. The Git backend uses this to indicate the type of file (normal,
    executable or symlink). *)
module Metadata : sig
  module type S = S.METADATA

  module None : S with type t = unit
  (** A metadata definition for systems that don't use metadata. *)
end

(** [Contents] specifies how user-defined contents need to be {e serializable}
    and {e mergeable}.

    The user needs to provide:

    - a type [t] to be used as store contents.
    - a value type for [t] (built using the {{!Irmin.Type} Irmin.Type}
      combinators).
    - a 3-way [merge] function, to handle conflicts between multiple versions of
      the same contents.

    Default implementations for {{!Contents.String} idempotent string} and
    {{!Contents.Json} JSON} contents are provided. *)
module Contents : sig
  module type S = S.CONTENTS

  module String : S with type t = string
  (** Contents of type [string], with the {{!Irmin.Merge.default} default} 3-way
      merge strategy: assume that update operations are idempotent and conflict
      iff values are modified concurrently. *)

  type json =
    [ `Null
    | `Bool of bool
    | `String of string
    | `Float of float
    | `O of (string * json) list
    | `A of json list ]

  module Json : S with type t = (string * json) list
  (** [Json] contents are associations from strings to [json] values stored as
      JSON encoded strings. If the same JSON key has been modified concurrently
      with different values then the [merge] function conflicts. *)

  module Json_value : S with type t = json
  (** [Json_value] allows any kind of json value to be stored, not only objects. *)

  module V1 : sig
    module String : S with type t = string
    (** Same as {!String} but use v1 serialisation format. *)
  end

  module type STORE = S.CONTENTS_STORE
  (** Contents store. *)

  (** [Store] creates a contents store. *)
  module Store (S : sig
    include CONTENT_ADDRESSABLE_STORE

    module Key : Hash.S with type t = key

    module Val : S with type t = value
  end) :
    STORE with type 'a t = 'a S.t and type key = S.key and type value = S.value
end

(** User-defined branches. *)
module Branch : sig
  (** {1 Branches} *)

  module type S = S.BRANCH
  (** The signature for branches. Irmin branches are similar to Git branches:
      they are used to associated user-defined names to head commits. Branches
      have a default value: the {{!Branch.S.master} master} branch. *)

  module String : S with type t = string
  (** [String] is an implementation of {{!Branch.S} S} where branches are
      strings. The [master] branch is ["master"]. Valid branch names contain
      only alpha-numeric characters, [-], [_], [.], and [/]. *)

  module type STORE = S.BRANCH_STORE
  (** [STORE] specifies the signature for branch stores.

      A {i branch store} is a mutable and reactive key / value store, where keys
      are branch names created by users and values are keys are head commmits. *)
end

type remote = S.remote = ..
(** The type for remote stores. *)

type config = S.config
(** The type for backend-specific configuration values.

    Every backend has different configuration options, which are kept abstract
    to the user. *)

(** [Private] defines functions only useful for creating new backends. If you
    are just using the library (and not developing a new backend), you should
    not use this module. *)
module Private : sig
  module Conf : module type of Conf
  (** Backend configuration.

      A backend configuration is a set of {{!keys} keys} mapping to typed
      values. Backends define their own keys. *)

  module Watch = Watch
  (** [Watch] provides helpers to register event notifications on read-write
      stores. *)

  module Lock = Lock

  (** [Node] provides functions to describe the graph-like structured values.

      The node blocks form a labeled directed acyclic graph, labeled by
      {{!Path.S.step} steps}: a list of steps defines a unique path from one
      node to an other.

      Each node can point to user-defined {{!Contents.S} contents} values. *)
  module Node : sig
    module type S = S.NODE

    (** [Make] provides a simple node implementation, parameterized by the
        contents and notes keys [K], paths [P] and metadata [M]. *)
    module Make
        (K : Type.S) (P : sig
          type step

          val step_t : step Type.t
        end)
        (M : Metadata.S) :
      S with type hash = K.t and type step = P.step and type metadata = M.t

    (** v1 serialisation *)
    module V1 (S : S) : sig
      include
        S
          with type hash = S.hash
           and type step = S.step
           and type metadata = S.metadata

      val import : S.t -> t

      val export : t -> S.t
    end

    module type STORE = S.NODE_STORE
    (** [STORE] specifies the signature for node stores. *)

    (** [Store] creates node stores. *)
    module Store
        (C : Contents.STORE)
        (P : Path.S)
        (M : Metadata.S) (S : sig
          include CONTENT_ADDRESSABLE_STORE with type key = C.key

          module Key : Hash.S with type t = key

          module Val :
            S
              with type t = value
               and type hash = key
               and type metadata = M.t
               and type step = P.step
        end) :
      STORE
        with type 'a t = 'a C.t * 'a S.t
         and type key = S.key
         and type value = S.value
         and module Path = P
         and module Metadata = M
         and type Key.t = S.Key.t
         and module Val = S.Val

    module type GRAPH = S.NODE_GRAPH
    (** [Graph] specifies the signature for node graphs. A node graph is a
        deterministic DAG, labeled by steps. *)

    module Graph (S : STORE) :
      GRAPH
        with type 'a t = 'a S.t
         and type contents = S.Contents.key
         and type metadata = S.Val.metadata
         and type node = S.key
         and type path = S.Path.t
         and type step = S.Path.step
  end

  (** Commit values represent the store history.

      Every commit contains a list of predecessor commits, and the collection of
      commits form an acyclic directed graph.

      Every commit also can contain an optional key, pointing to a
      {{!Private.Commit.STORE} node} value. See the {{!Private.Node.STORE} Node}
      signature for more details on node values. *)
  module Commit : sig
    module type S = S.COMMIT

    (** [Make] provides a simple implementation of commit values, parameterized
        by the commit and node keys [K]. *)
    module Make (K : Type.S) : S with type hash = K.t

    (** V1 serialisation. *)
    module V1 (S : S) : sig
      include S with type hash = S.hash

      val import : S.t -> t

      val export : t -> S.t
    end

    module type STORE = S.COMMIT_STORE
    (** [STORE] specifies the signature for commit stores. *)

    (** [Store] creates a new commit store. *)
    module Store
        (N : Node.STORE) (S : sig
          include CONTENT_ADDRESSABLE_STORE with type key = N.key

          module Key : Hash.S with type t = key

          module Val : S with type t = value and type hash = key
        end) :
      STORE
        with type 'a t = 'a N.t * 'a S.t
         and type key = S.key
         and type value = S.value
         and type Key.t = S.Key.t
         and module Val = S.Val

    module type HISTORY = S.COMMIT_HISTORY
    (** [History] specifies the signature for commit history. The history is
        represented as a partial-order of commits and basic functions to search
        through that history are provided.

        Every commit can point to an entry point in a node graph, where
        user-defined contents are stored. *)

    (** Build a commit history. *)
    module History (S : STORE) :
      HISTORY
        with type 'a t = 'a S.t
         and type node = S.Node.key
         and type commit = S.key
  end

  (** The signature for slices. *)
  module Slice : sig
    module type S = S.SLICE

    (** Build simple slices. *)
    module Make (C : Contents.STORE) (N : Node.STORE) (H : Commit.STORE) :
      S
        with type contents = C.key * C.value
         and type node = N.key * N.value
         and type commit = H.key * H.value
  end

  module Sync : sig
    module type S = S.SYNC

    (** [None] is an implementation of {{!Private.Sync.S} S} which does nothing. *)
    module None (H : Type.S) (B : Type.S) : sig
      include S with type commit = H.t and type branch = B.t

      val v : 'a -> t Lwt.t
      (** Create a remote store handle. *)
    end
  end

  (** The complete collection of private implementations. *)
  module type S = sig
    (** {1 Private Implementations} *)

    module Hash : Hash.S
    (** Internal hashes. *)

    module Contents : Contents.STORE with type key = Hash.t
    (** Private content store. *)

    (** Private node store. *)
    module Node :
      Node.STORE with type key = Hash.t and type Val.hash = Contents.key

    (** Private commit store. *)
    module Commit :
      Commit.STORE with type key = Hash.t and type Val.hash = Node.key

    module Branch : Branch.STORE with type value = Commit.key
    (** Private branch store. *)

    (** Private slices. *)
    module Slice :
      Slice.S
        with type contents = Contents.key * Contents.value
         and type node = Node.key * Node.value
         and type commit = Commit.key * Commit.value

    (** Private repositories. *)
    module Repo : sig
      type t

      val v : config -> t Lwt.t

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

    (** URI-based low-level sync. *)
    module Sync : sig
      include Sync.S with type commit = Commit.key and type branch = Branch.key

      val v : Repo.t -> t Lwt.t
    end
  end
end

(** {1 High-level Stores}

    An Irmin store is a branch-consistent store where keys are lists of steps.

    An example is a Git repository where keys are filenames, {e i.e.} lists of
    ['/']-separated strings. More complex examples are structured values, where
    steps might contain first-class field accessors and array offsets.

    Irmin provides the following features:

    - Support for fast clones, branches and merges, in a fashion very similar to
      Git.
    - Efficient staging areas for fast, transient, in-memory operations.
    - Fast {{!Sync} synchronization} primitives between remote stores, using
      native backend protocols (as the Git protocol) when available. *)

exception Closed
(** The exception raised when any operation is attempted on a closed store,
    except for {!S.close}, which is idempotent. *)

(** Irmin stores. *)
module type S = sig
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

  (** [Status] provides base functions for store statuses. *)
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
      t ->
      ?max_depth:int ->
      ?n:int ->
      commit ->
      (unit, [ `No_change | `Rejected | lca_error ]) result Lwt.t
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

  module Hash : Hash.S with type t = hash
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
    include Contents.S with type t = contents

    (** {1 Import/Export} *)

    val hash : contents -> hash
    (** [hash c] it [c]'s hash in the repository [r]. *)

    val of_hash : repo -> hash -> contents option Lwt.t
    (** [of_hash r h] is the the contents object in [r] having [h] as hash, or
        [None] is no such contents object exists. *)
  end

  (** Managing store's trees. *)

  module Tree : sig
    (** [Tree] provides immutable, in-memory partial mirror of the store, with
        lazy reads and delayed writes.

        Trees are like staging area in Git: they are immutable temporary
        non-persistent areas (they disappear if the host crash), held in memory
        for efficiency, where reads are done lazily and writes are done only
        when needed on commit: if you modify a key twice, only the last change
        will be written to the store when you commit. *)

    (** {1 Constructors} *)

    val empty : tree
    (** [empty] is the empty tree. The empty tree does not have associated
        backend configuration values, as they can perform in-memory operation,
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
    (** Same as {!find_all} but raise [Invalid_arg] if [k] is not present in
        [t]. *)

    val get : tree -> key -> contents Lwt.t
    (** Same as {!get_all} but ignore the metadata. *)

    val add : tree -> key -> ?metadata:metadata -> contents -> tree Lwt.t
    (** [add t k c] is the tree where the key [k] is bound to the contents [c]
        but is similar to [t] for other bindings. *)

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
    (** The type for {!fold}'s [force] parameter. [`True] forces the fold to
        read the objects of the lazy nodes. [`False f] is applying [f] on every
        lazy node instead. *)

    type uniq = [ `False | `True | `Marks of marks ]
    (** The type for {!fold}'s [uniq] parameters. [`False] folds over all the
        nodes. [`True] does not recurse on nodes already seen. [`Marks m] uses
        the collection of marks [m] to store the cache of keys: the fold will
        modify [m]. This can be used for incremental folds. *)

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
    (** [stats ~force t] are [t]'s statistics. If [force] is true, this will
        force the reading of lazy nodes. By default it is [false]. *)

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
    (** [clear ?depth t] clears all the cache in the tree [t] for subtrees with
        a depth higher than [depth]. If [depth] is not set, all the subtrees are
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

    (** {1 Import/Export} *)

    val hash : tree -> hash
    (** [hash r c] it [c]'s hash in the repository [r]. *)

    val of_hash : repo -> hash -> tree option Lwt.t
    (** [of_hash r h] is the the tree object in [r] having [h] as hash, or
        [None] is no such tree object exists. *)

    val shallow : repo -> hash -> tree
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

    include Branch.S with type t = branch
    (** Base functions for branches. *)
  end

  (** [Key] provides base functions for the stores's paths. *)
  module Key : Path.S with type t = key and type step = step

  module Metadata : Metadata.S with type t = metadata
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

  val kind_t : [ `Node | `Contents ] Type.t
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
      Private.S
        with type Contents.value = contents
         and module Node.Path = Key
         and type Hash.t = Hash.t
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
  (** [to_private_node n] is the private node objects built using [n]. The
      operation can fetch the database to read an object as [n] could be
      represented as a hash. The result is [None] iff that hash doesn't exist in
      the database. *)

  val of_private_node : repo -> Private.Node.value -> node
  (** [of_private_node r n] is the node build from the private node object [n]. *)

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

(** [Json_tree] is used to project JSON values onto trees. Instead of the entire
    object being stored under one key, it is split across several keys starting
    at the specified root key. *)
module Json_tree (Store : S with type contents = Contents.json) : sig
  include Contents.S with type t = Contents.json

  val to_concrete_tree : t -> Store.Tree.concrete

  val of_concrete_tree : Store.Tree.concrete -> t

  val get_tree : Store.tree -> Store.key -> t Lwt.t
  (** Extract a [json] value from tree at the given key. *)

  val set_tree : Store.tree -> Store.key -> t -> Store.tree Lwt.t
  (** Project a [json] value onto a tree at the given key. *)

  val get : Store.t -> Store.key -> t Lwt.t
  (** Extract a [json] value from a store at the given key. *)

  val set : Store.t -> Store.key -> t -> info:Info.f -> unit Lwt.t
  (** Project a [json] value onto a store at the given key. *)
end

(** [S_MAKER] is the signature exposed by any backend providing {!S}
    implementations. [M] is the implementation of user-defined metadata, [C] is
    the one for user-defined contents, [B] is the implementation for branches
    and [H] is the implementation for object (blobs, trees, commits) hashes. It
    does not use any native synchronization primitives. *)
module type S_MAKER = functor
  (M : Metadata.S)
  (C : Contents.S)
  (P : Path.S)
  (B : Branch.S)
  (H : Hash.S)
  ->
  S
    with type key = P.t
     and type step = P.step
     and type metadata = M.t
     and type contents = C.t
     and type branch = B.t
     and type hash = H.t

(** [KV] is similar to {!S} but chooses sensible implementations for path and
    branch. *)
module type KV =
  S with type key = string list and type step = string and type branch = string

(** [KV_MAKER] is like {!S_MAKER} but where everything except the contents is
    replaced by sensible default implementations. *)
module type KV_MAKER = functor (C : Contents.S) -> KV with type contents = C.t

(** {2 Synchronization} *)

val remote_store : (module S with type t = 'a) -> 'a -> remote
(** [remote_store t] is the remote corresponding to the local store [t].
    Synchronization is done by importing and exporting store {{!BC.slice}
    slices}, so this is usually much slower than native synchronization using
    {!Store.remote} but it works for all backends. *)

module type SYNC = S.SYNC_STORE
(** [SYNC] provides functions to synchronize an Irmin store with local and
    remote Irmin stores. *)

(** The default [Sync] implementation. *)
module Sync (S : S) : SYNC with type db = S.t and type commit = S.commit

(** {1:examples Examples}

    These examples are in the [examples] directory of the distribution.

    {3 Syncing with a remote}

    A simple synchronization example, using the {{!Irmin_unix.Git} Git} backend
    and the {!Sync} helpers. The code clones a fresh repository if the
    repository does not exist locally, otherwise it performs a fetch: in this
    case, only the missing contents are downloaded.

    {[
      open Lwt.Infix
      module S = Irmin_unix.Git.FS.KV (Irmin.Contents.String)
      module Sync = Irmin.Sync (S)

      let config = Irmin_git.config "/tmp/test"

      let upstream =
        if Array.length Sys.argv = 2 then
          Uri.of_string (Store.remote Sys.argv.(1))
        else (
          Printf.eprintf "Usage: sync [uri]\n%!";
          exit 1 )

      let test () =
        S.Repo.v config >>= S.master >>= fun t ->
        Sync.pull_exn t upstream `Set >>= fun () ->
        S.get t [ "README.md" ] >|= fun r -> Printf.printf "%s\n%!" r

      let () = Lwt_main.run (test ())
    ]}

    {3 Mergeable logs}

    We will demonstrate the use of custom merge operators by defining mergeable
    debug log files. We first define a log entry as a pair of a timestamp and a
    message, using the combinator exposed by {!Irmin.Type}:

    {[
      module Entry : sig
        include Irmin.Type.S

        val v : string -> t

        val timestamp : t -> int
      end = struct
        type t = { timestamp : int; message : string }

        let compare x y = compare x.timestamp y.timestamp

        let time = ref 0

        let v message =
          incr time;
          { timestamp = !time; message }

        let timestamp t = t.timestamp

        let pp ppf { timestamp; message } =
          Fmt.pf ppf "%04d: %s" timestamp message

        let of_string str =
          match String.split_on_char '\t' str with
          | [] -> Error (`Msg ("invalid entry: " ^ str))
          | ts :: msg_sects -> (
              let message = String.concat "\t" msg_sects in
              try Ok { timestamp = int_of_string ts; message }
              with Failure e -> Error (`Msg e) )

        let t =
          let open Irmin.Type in
          record "entry" (fun t32 message ->
              { timestamp = Int32.to_int t32; message })
          |+ field "timestamp" int32 (fun t -> Int32.of_int t.timestamp)
          |+ field "message" string (fun t -> t.message)
          |> sealr

        let t = Irmin.Type.like ~cli:(pp, of_string) ~compare t
      end
    ]}

    A log file is a list of entries (one per line), ordered by decreasing order
    of timestamps. The 3-way [merge] operator for log files concatenates and
    sorts the new entries and prepend them to the common ancestor's ones.

    {[
      (* A log file *)
      module Log : sig
        include Irmin.Contents.S

        val add : t -> Entry.t -> t

        val empty : t
      end = struct
        type t = Entry.t list

        let empty = []

        let pp ppf l = List.iter (Fmt.pf ppf "%a\n" Entry.pp) (List.rev l)

        let of_string str =
          let lines = String.cuts ~empty:false ~sep:"\n" str in
          try
            List.fold_left
              (fun acc l ->
                match Entry.of_string l with
                | Ok x -> x :: acc
                | Error (`Msg e) -> failwith e)
              [] lines
            |> fun l -> Ok l
          with Failure e -> Error (`Msg e)

        let t = Irmin.Type.(list Entry.t)

        let t = Irmin.Type.like' ~cli:(pp, of_string) t

        let timestamp = function [] -> 0 | e :: _ -> Entry.timestamp e

        let newer_than timestamp file =
          let rec aux acc = function
            | [] -> List.rev acc
            | h :: _ when Entry.timestamp h <= timestamp -> List.rev acc
            | h :: t -> aux (h :: acc) t
          in
          aux [] file

        let merge ~old t1 t2 =
          let open Irmin.Merge.Infix in
          old () >>=* fun old ->
          let old = match old with None -> [] | Some o -> o in
          let ts = timestamp old in
          let t1 = newer_than ts t1 in
          let t2 = newer_than ts t2 in
          let t3 = List.sort Entry.compare (List.rev_append t1 t2) in
          Irmin.Merge.ok (List.rev_append t3 old)

        let merge = Irmin.Merge.(option (v t merge))

        let add t e = e :: t
      end
    ]}

    {b Note:} The serialisation primitives used in that example are not very
    efficient in this case as they parse the file every time. For real usage,
    you would write buffered versions of [Log.pp] and [Log.of_string].

    To persist the log file on disk, we need to choose a backend. We show here
    how to use the on-disk [Git] backend on Unix.

    {[
      (* Build an Irmin store containing log files. *)
      module S = Irmin_unix.Git.FS.KV (Log)

      (* Set-up the local configuration of the Git repository. *)
      let config = Irmin_git.config ~bare:true "/tmp/irmin/test"

      (* Set-up the commit info function *)
      let info fmt = Irmin_unix.info ~author:"logger" fmt
    ]}

    We can now define a toy example to use our mergeable log files.

    {[
      open Lwt.Infix

      (* Name of the log file. *)
      let file = [ "local"; "debug" ]

      (* Read the entire log file. *)
      let read_file t = S.find t file >|= function None -> [] | Some l -> l

      (* Persist a new entry in the log. *)
      let log t fmt =
        Fmt.kstrf
          (fun message ->
            read_file t >>= fun logs ->
            let logs = Log.add logs (Entry.v message) in
            S.set t (info "Adding a new entry") file logs)
          fmt

      let () =
        Lwt_main.run
          ( S.Repo.v config >>= S.master >>= fun t ->
            log t "Adding a new log entry" >>= fun () ->
            Irmin.clone_force ~src:t ~dst:"x" >>= fun x ->
            log x "Adding new stuff to x" >>= fun () ->
            log x "Adding more stuff to x" >>= fun () ->
            log x "More. Stuff. To x." >>= fun () ->
            log t "I can add stuff on t also" >>= fun () ->
            log t "Yes. On t!" >>= fun () ->
            S.merge (info "Merging x into t") x ~into:t >|= function
            | Ok () -> ()
            | Error _ -> failwith "merge conflict!" )
    ]} *)

(** {1 Helpers} *)

(** [Dot] provides functions to export a store to the Graphviz `dot` format. *)
module Dot (S : S) : Dot.S with type db = S.t

(** {1:backend Backends}

    API to create new Irmin backends. A backend is an implementation exposing
    either a concrete implementation of {!S} or a functor providing {!S} once
    applied.

    There are two ways to create a concrete {!Irmin.S} implementation:

    - {!Make} creates a store where all the objects are stored in the same
      store, using the same internal keys format and a custom binary format
      based on {{:https://github.com/janestreet/bin_prot} bin_prot}, with no
      native synchronization primitives: it is usually what is needed to quickly
      create a new backend.
    - {!Make_ext} creates a store with a {e deep} embedding of each of the
      internal stores into separate store, with total control over the binary
      format and using the native synchronization protocols when available. *)

(** [APPEND_ONLY_STORE_MAKER] is the signature exposed by append-only store
    backends. [K] is the implementation of keys and [V] is the implementation of
    values. *)
module type APPEND_ONLY_STORE_MAKER = functor (K : Type.S) (V : Type.S) -> sig
  include APPEND_ONLY_STORE with type key = K.t and type value = V.t

  val batch : [ `Read ] t -> ([ `Read | `Write ] t -> 'a Lwt.t) -> 'a Lwt.t
  (** [batch t f] applies the writes in [f] in a separate batch. The exact
      guarantees depends on the backends. *)

  val v : config -> [ `Read ] t Lwt.t
  (** [v config] is a function returning fresh store handles, with the
      configuration [config], which is provided by the backend. *)

  val close : 'a t -> unit Lwt.t
  (** [close t] frees up all the resources associated to [t]. Any operations run
      on a closed store will raise {!Closed}. *)
end

(** [CONTENT_ADDRESSABLE_STOREMAKER] is the signature exposed by
    content-addressable store backends. [K] is the implementation of keys and
    [V] is the implementation of values. *)
module type CONTENT_ADDRESSABLE_STORE_MAKER = functor
  (K : Hash.S)
  (V : Type.S)
  -> sig
  include CONTENT_ADDRESSABLE_STORE with type key = K.t and type value = V.t

  val batch : [ `Read ] t -> ([ `Read | `Write ] t -> 'a Lwt.t) -> 'a Lwt.t
  (** [batch t f] applies the writes in [f] in a separate batch. The exact
      guarantees depends on the backends. *)

  val v : config -> [ `Read ] t Lwt.t
  (** [v config] is a function returning fresh store handles, with the
      configuration [config], which is provided by the backend. *)

  val close : 'a t -> unit Lwt.t
  (** [close t] frees up all the resources associated to [t]. Any operations run
      on a closed store will raise {!Closed}. *)
end

module Content_addressable
    (S : APPEND_ONLY_STORE_MAKER)
    (K : Hash.S)
    (V : Type.S) : sig
  include
    CONTENT_ADDRESSABLE_STORE
      with type 'a t = 'a S(K)(V).t
       and type key = K.t
       and type value = V.t

  val batch : [ `Read ] t -> ([ `Read | `Write ] t -> 'a Lwt.t) -> 'a Lwt.t
  (** [batch t f] applies the writes in [f] in a separate batch. The exact
      guarantees depends on the backends. *)

  val v : config -> [ `Read ] t Lwt.t
  (** [v config] is a function returning fresh store handles, with the
      configuration [config], which is provided by the backend. *)

  val close : 'a t -> unit Lwt.t
  (** [close t] frees up all the resources associated to [t]. Any operations run
      on a closed store will raise {!Closed}. *)
end

(** [ATOMIC_WRITE_STORE_MAKER] is the signature exposed by atomic-write store
    backends. [K] is the implementation of keys and [V] is the implementation of
    values.*)
module type ATOMIC_WRITE_STORE_MAKER = functor (K : Type.S) (V : Type.S) -> sig
  include ATOMIC_WRITE_STORE with type key = K.t and type value = V.t

  val v : config -> t Lwt.t
  (** [v config] is a function returning fresh store handles, with the
      configuration [config], which is provided by the backend. *)
end

(** Simple store creator. Use the same type of all of the internal keys and
    store all the values in the same store. *)
module Make
    (CA : CONTENT_ADDRESSABLE_STORE_MAKER)
    (AW : ATOMIC_WRITE_STORE_MAKER) : S_MAKER

module Make_ext
    (CA : CONTENT_ADDRESSABLE_STORE_MAKER)
    (AW : ATOMIC_WRITE_STORE_MAKER)
    (Metadata : Metadata.S)
    (Contents : Contents.S)
    (Path : Path.S)
    (Branch : Branch.S)
    (Hash : Hash.S)
    (Node : Private.Node.S
              with type metadata = Metadata.t
               and type hash = Hash.t
               and type step = Path.step)
    (Commit : Private.Commit.S with type hash = Hash.t) :
  S
    with type key = Path.t
     and type contents = Contents.t
     and type branch = Branch.t
     and type hash = Hash.t
     and type step = Path.step
     and type metadata = Metadata.t
     and type Key.step = Path.step

(** Advanced store creator. *)
module Of_private (P : Private.S) :
  S
    with type key = P.Node.Path.t
     and type contents = P.Contents.value
     and type branch = P.Branch.key
     and type hash = P.Hash.t
     and type step = P.Node.Path.step
     and type metadata = P.Node.Val.metadata
     and type Key.step = P.Node.Path.step
     and type repo = P.Repo.t
     and type slice = P.Slice.t
     and module Private = P
