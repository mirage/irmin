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

open! Import
module Sigs = S
open Sigs.Store_properties

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

  type +'a io
  (** The type for IO effects. *)

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

  type tree
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

  module IO : IO.S with type 'a t = 'a io

  (** IO Effects. *)
  module Merge : Merge.S with type 'a io = 'a io
  (** Merge combinators. *)

  (** Repositories. *)
  module Repo : sig
    (** {1 Repositories}

        A repository contains a set of branches. *)

    type t = repo
    (** The type of repository handles. *)

    val v : S.config -> t io
    (** [v config] connects to a repository in a backend-specific manner. *)

    (** @inline *)
    include CLOSEABLE with type _ t := t and type 'a io := 'a io

    val heads : t -> commit list io
    (** [heads] is {!Head.list}. *)

    val branches : t -> branch list io
    (** [branches] is {!Branch.list}. *)

    val export :
      ?full:bool ->
      ?depth:int ->
      ?min:commit list ->
      ?max:[ `Head | `Max of commit list ] ->
      t ->
      slice io
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

    val import : t -> slice -> (unit, [ `Msg of string ]) result io
    (** [import t s] imports the contents of the slice [s] in [t]. Does not
        modify branches. *)

    type elt =
      [ `Commit of hash | `Node of hash | `Contents of hash | `Branch of branch ]
    [@@deriving irmin]
    (** The type for elements iterated over by {!iter}. *)

    val iter :
      ?cache_size:int ->
      min:elt list ->
      max:elt list ->
      ?edge:(elt -> elt -> unit io) ->
      ?branch:(branch -> unit io) ->
      ?commit:(hash -> unit io) ->
      ?node:(hash -> unit io) ->
      ?contents:(hash -> unit io) ->
      ?skip_branch:(branch -> bool io) ->
      ?skip_commit:(hash -> bool io) ->
      ?skip_node:(hash -> bool io) ->
      ?skip_contents:(hash -> bool io) ->
      ?pred_branch:(t -> branch -> elt list io) ->
      ?pred_commit:(t -> hash -> elt list io) ->
      ?pred_node:(t -> hash -> elt list io) ->
      ?pred_contents:(t -> hash -> elt list io) ->
      ?rev:bool ->
      t ->
      unit io
    (** [iter t] iterates in topological order over the closure graph of [t]. If
        [rev] is set (by default it is) the traversal is done in reverse order.

        [skip_branch], [skip_commit], [skip_node] and [skip_contents] allow the
        traversal to be stopped when the corresponding objects are traversed. By
        default no objects are skipped.

        The [branch], [commit], [node] and [contents] functions are called
        whenever the corresponding objects are traversed. By default these
        functions do nothing. These functions are not called on skipped objects.

        [pred_branch], [pred_commit], [pred_node] and [pred_contents] implicitly
        define the graph underlying the traversal. By default they exactly match
        the underlying Merkle graph of the repository [t]. These functions can
        be used to traverse a slightly modified version of that graph, for
        instance by modifying [pred_contents] to implicitly link structured
        contents with other objects in the graph.

        The traversed objects are all included between [min] (included) and
        [max] (included), following the Merkle graph order. Moreover, the [min]
        boundary is extended as follows:

        - contents and node objects in [min] stop the traversal; their
          predecessors are not traversed.
        - commit objects in [min] stop the traversal for their commit
          predecessors, but their sub-node are still traversed. This allows
          users to define an inclusive range of commit to iterate over.
        - branch objects in [min] implicitly add to [min] the commit they are
          pointing to; this allow users to define the iteration between two
          branches.

        [cache_size] is the size of the LRU used to store traversed objects. If
        an entry is evicted from the LRU, it can be traversed multiple times by
        {!Repo.iter}. When [cache_size] is [None] (the default), no entries is
        ever evicted from the cache; hence every object is only traversed once,
        at the cost of having to store all the traversed objects in memory. *)
  end

  val empty : repo -> t io
  (** [empty repo] is a temporary, empty store. Becomes a normal temporary store
      after the first update. *)

  val master : repo -> t io
  (** [master repo] is a persistent store based on [r]'s master branch. This
      operation is cheap, can be repeated multiple times. *)

  val of_branch : repo -> branch -> t io
  (** [of_branch r name] is a persistent store based on the branch [name].
      Similar to [master], but use [name] instead {!Branch.S.master}. *)

  val of_commit : commit -> t io
  (** [of_commit c] is a temporary store, based on the commit [c].

      Temporary stores do not have stable names: instead they can be addressed
      using the hash of the current commit. Temporary stores are similar to
      Git's detached heads. In a temporary store, all the operations are
      performed relative to the current head and update operations can modify
      the current head: the current stores's head will automatically become the
      new head obtained after performing the update. *)

  val repo : t -> repo
  (** [repo t] is the repository containing [t]. *)

  val tree : t -> tree io
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
    val list : repo -> commit list io
    (** [list t] is the list of all the heads in local store. Similar to
        [git rev-list --all]. *)

    val find : t -> commit option io
    (** [find t] is the current head of the store [t]. This works for both
        persistent and temporary branches. In the case of a persistent branch,
        this involves getting the the head associated with the branch, so this
        may block. In the case of a temporary store, it simply returns the
        current head. Returns [None] if the store has no contents. Similar to
        [git rev-parse HEAD]. *)

    val get : t -> commit io
    (** Same as {!find} but raise [Invalid_argument] if the store does not have
        any contents. *)

    val set : t -> commit -> unit io
    (** [set t h] updates [t]'s contents with the contents of the commit [h].
        Can cause data loss as it discards the current contents. Similar to
        [git reset --hard <hash>]. *)

    val fast_forward :
      t -> ?max_depth:int -> ?n:int -> commit -> (unit, ff_error) result io
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

    val test_and_set : t -> test:commit option -> set:commit option -> bool io
    (** Same as {!update_head} but check that the value is [test] before
        updating to [set]. Use {!update} or {!merge} instead if possible. *)

    val merge :
      into:t ->
      info:Info.f ->
      ?max_depth:int ->
      ?n:int ->
      commit ->
      (unit, Merge.conflict) result io
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

    val v : repo -> info:Info.t -> parents:hash list -> tree -> commit io
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

    val of_hash : repo -> hash -> commit option io
    (** [of_hash r h] is the the commit object in [r] having [h] as hash, or
        [None] is no such commit object exists. *)
  end

  (** [Contents] provides base functions for the store's contents. *)
  module Contents : sig
    include Contents.S with type t = contents with type 'a merge := 'a Merge.t

    (** {1 Import/Export} *)

    val hash : contents -> hash
    (** [hash c] it [c]'s hash in the repository [r]. *)

    val of_hash : repo -> hash -> contents option io
    (** [of_hash r h] is the the contents object in [r] having [h] as hash, or
        [None] is no such contents object exists. *)
  end

  (** Managing store's trees. *)
  module Tree : sig
    include
      Tree.S
        with type t := tree
         and type step := step
         and type key := key
         and type metadata := metadata
         and type contents := contents
         and type node := node
         and type hash := hash
         and type 'a io := 'a io
         and type 'a merge := 'a Merge.t

    (** {1 Import/Export} *)

    val hash : tree -> hash
    (** [hash r c] it [c]'s hash in the repository [r]. *)

    type kinded_hash := [ `Contents of hash * metadata | `Node of hash ]
    (** Hashes in the Irmin store are tagged with the type of the value they
        reference (either {!contents} or {!node}). In the [contents] case, the
        hash is paired with corresponding {!metadata}. *)

    val of_hash : Repo.t -> kinded_hash -> tree option io
    (** [of_hash r h] is the the tree object in [r] having [h] as hash, or
        [None] is no such tree object exists. *)

    val shallow : Repo.t -> kinded_hash -> tree
    (** [shallow r h] is the shallow tree object with the hash [h]. No check is
        performed to verify if [h] actually exists in [r]. *)
  end

  (** {1 Reads} *)

  val kind : t -> key -> [ `Contents | `Node ] option io
  (** [kind] is {!Tree.kind} applied to [t]'s root tree. *)

  val list : t -> key -> (step * tree) list io
  (** [list t] is {!Tree.list} applied to [t]'s root tree. *)

  val mem : t -> key -> bool io
  (** [mem t] is {!Tree.mem} applied to [t]'s root tree. *)

  val mem_tree : t -> key -> bool io
  (** [mem_tree t] is {!Tree.mem_tree} applied to [t]'s root tree. *)

  val find_all : t -> key -> (contents * metadata) option io
  (** [find_all t] is {!Tree.find_all} applied to [t]'s root tree. *)

  val find : t -> key -> contents option io
  (** [find t] is {!Tree.find} applied to [t]'s root tree. *)

  val get_all : t -> key -> (contents * metadata) io
  (** [get_all t] is {!Tree.get_all} applied on [t]'s root tree. *)

  val get : t -> key -> contents io
  (** [get t] is {!Tree.get} applied to [t]'s root tree. *)

  val find_tree : t -> key -> tree option io
  (** [find_tree t] is {!Tree.find_tree} applied to [t]'s root tree. *)

  val get_tree : t -> key -> tree io
  (** [get_tree t k] is {!Tree.get_tree} applied to [t]'s root tree. *)

  val hash : t -> key -> hash option io
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
    (unit, write_error) result io
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
    unit io
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
    (unit, write_error) result io
  (** [set_tree] is like {!set} but for trees. *)

  val set_tree_exn :
    ?retries:int ->
    ?allow_empty:bool ->
    ?parents:commit list ->
    info:Info.f ->
    t ->
    key ->
    tree ->
    unit io
  (** [set_tree] is like {!set_exn} but for trees. *)

  val remove :
    ?retries:int ->
    ?allow_empty:bool ->
    ?parents:commit list ->
    info:Info.f ->
    t ->
    key ->
    (unit, write_error) result io
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
    unit io
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
    (unit, write_error) result io
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
    unit io
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
    (unit, write_error) result io
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
    unit io
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
    (unit, write_error) result io
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
    unit io
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
    (unit, write_error) result io
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
    unit io
  (** [merge_tree] is like {!merge_tree} but for trees. *)

  val with_tree :
    ?retries:int ->
    ?allow_empty:bool ->
    ?parents:commit list ->
    ?strategy:[ `Set | `Test_and_set | `Merge ] ->
    info:Info.f ->
    t ->
    key ->
    (tree option -> tree option io) ->
    (unit, write_error) result io
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
    (tree option -> tree option io) ->
    unit io
  (** [with_tree_exn] is like {!with_tree} but raise [Failure _] instead of
      using a return type. *)

  (** {1 Clones} *)

  val clone : src:t -> dst:branch -> t io
  (** [clone ~src ~dst] makes [dst] points to [Head.get src]. [dst] is created
      if needed. Remove the current contents en [dst] if [src] is {!empty}. *)

  (** {1 Watches} *)

  type watch
  (** The type for store watches. *)

  val watch : t -> ?init:commit -> (commit S.diff -> unit io) -> watch io
  (** [watch t f] calls [f] every time the contents of [t]'s head is updated.

      {b Note:} even if [f] might skip some head updates, it will never be
      called concurrently: all consecutive calls to [f] are done in sequence, so
      we ensure that the previous one ended before calling the next one. *)

  val watch_key :
    t -> key -> ?init:commit -> ((commit * tree) S.diff -> unit io) -> watch io
  (** [watch_key t key f] calls [f] every time the [key]'s value is added,
      removed or updated. If the current branch is deleted, no signal is sent to
      the watcher. *)

  val unwatch : watch -> unit io
  (** [unwatch w] disable [w]. Return once the [w] is fully disabled. *)

  (** {1 Merges and Common Ancestors.} *)

  type 'a merge =
    info:Info.f ->
    ?max_depth:int ->
    ?n:int ->
    'a ->
    (unit, Merge.conflict) result io
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
    ?max_depth:int -> ?n:int -> t -> t -> (commit list, lca_error) result io
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
    (commit list, lca_error) result io
  (** Same as {!lcas} but takes a branch ID as argument. *)

  val lcas_with_commit :
    t ->
    ?max_depth:int ->
    ?n:int ->
    commit ->
    (commit list, lca_error) result io
  (** Same as {!lcas} but takes a commit ID as argument. *)

  (** {1 History} *)

  module History : Graph.Sig.P with type V.t = commit
  (** An history is a DAG of heads. *)

  val history :
    ?depth:int -> ?min:commit list -> ?max:commit list -> t -> History.t io
  (** [history ?depth ?min ?max t] is a view of the history of the store [t], of
      depth at most [depth], starting from the [t]'s head (or from [max] if the
      head is not set) and stopping at [min] if specified. *)

  val last_modified : ?depth:int -> ?n:int -> t -> key -> commit list io
  (** [last_modified ?number c k] is the list of the last [number] commits that
      modified [key], in ascending order of date. [depth] is the maximum depth
      to be explored in the commit graph, if any. Default value for [number] is
      1. *)

  (** Manipulate branches. *)
  module Branch : sig
    (** {1 Branch Store}

        Manipulate relations between {{!branch} branches} and {{!commit}
        commits}. *)

    val mem : repo -> branch -> bool io
    (** [mem r b] is true iff [b] is present in [r]. *)

    val find : repo -> branch -> commit option io
    (** [find r b] is [Some c] iff [c] is bound to [b] in [t]. It is [None] if
        [b] is not present in [t]. *)

    val get : repo -> branch -> commit io
    (** [get t b] is similar to {!find} but raise [Invalid_argument] if [b] is
        not present in [t]. *)

    val set : repo -> branch -> commit -> unit io
    (** [set t b c] bounds [c] to [b] in [t]. *)

    val remove : repo -> branch -> unit io
    (** [remove t b] removes [b] from [t]. *)

    val list : repo -> branch list io
    (** [list t] is the list of branches present in [t]. *)

    val watch :
      repo -> branch -> ?init:commit -> (commit S.diff -> unit io) -> watch io
    (** [watch t b f] calls [f] on every change in [b]. *)

    val watch_all :
      repo ->
      ?init:(branch * commit) list ->
      (branch -> commit S.diff -> unit io) ->
      watch io
    (** [watch_all t f] calls [f] on every branch-related change in [t],
        including creation/deletion events. *)

    include Branch.S with type t = branch
    (** Base functions for branches. *)
  end

  (** [Key] provides base functions for the stores's paths. *)
  module Key : Path.S with type t = key and type step = step

  (** [Metadata] provides base functions for node metadata. *)
  module Metadata :
    Metadata.S with type t = metadata and type 'a merge := 'a Merge.t

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
      Private.S
        with type 'a io := 'a io
         and type 'a merge := 'a Merge.t
         and type Contents.value = contents
         and module Hash = Hash
         and module Node.Path = Key
         and type Node.Metadata.t = metadata
         and type Branch.key = branch
         and type Slice.t = slice
         and type Repo.t = repo
         and module IO = IO
         and module Merge = Merge
  end

  (** Extend the [remote] type with [endpoint]. *)
  type S.remote += E of Private.Sync.endpoint

  (** {2 Converters to private types} *)

  val to_private_node : node -> Private.Node.value Tree.or_error io
  val of_private_node : repo -> Private.Node.value -> node

  val to_private_commit : commit -> Private.Commit.value
  (** [to_private_commit c] is the private commit object associated with the
      commit [c]. *)

  val of_private_commit : repo -> Private.Commit.value -> commit
  (** [of_private_commit r c] is the commit associated with the private commit
      object [c]. *)

  val save_contents : [> write ] Private.Contents.t -> contents -> hash io
  (** Save a content into the database *)

  val save_tree :
    ?clear:bool ->
    repo ->
    [> write ] Private.Contents.t ->
    [> read_write ] Private.Node.t ->
    tree ->
    hash io
  (** Save a tree into the database. Does not do any reads. If [clear] is set
      (it is by default), the tree cache will be cleared after the save. *)
end

module type MAKER = sig
  type +'a io
  type 'a merge

  module Make
      (M : Metadata.S with type 'a merge := 'a merge)
      (C : Contents.S with type 'a merge := 'a merge)
      (P : Path.S)
      (B : Branch.S)
      (H : Hash.S) :
    S
      with type 'a io := 'a io
       and type 'a Merge.t = 'a merge
       and type key = P.t
       and type step = P.step
       and type metadata = M.t
       and type contents = C.t
       and type branch = B.t
       and type hash = H.t
       and type Private.Sync.endpoint = unit
end

module type JSON_TREE = functor
  (Store : S with type contents = Contents.json)
  -> sig
  type 'a io := 'a Store.io
  type 'a merge := 'a Store.Merge.t

  include Contents.S with type t = Contents.json and type 'a merge := 'a merge

  val to_concrete_tree : t -> Store.Tree.concrete
  val of_concrete_tree : Store.Tree.concrete -> t

  val get_tree : Store.tree -> Store.key -> t io
  (** Extract a [json] value from tree at the given key. *)

  val set_tree : Store.tree -> Store.key -> t -> Store.tree io
  (** Project a [json] value onto a tree at the given key. *)

  val get : Store.t -> Store.key -> t io
  (** Extract a [json] value from a store at the given key. *)

  val set : Store.t -> Store.key -> t -> info:Info.f -> unit io
  (** Project a [json] value onto a store at the given key. *)
end

module type Store = sig
  module type S = S
  module type MAKER = MAKER
  module type JSON_TREE = JSON_TREE

  module Make (P : Private.S) :
    S
      with type 'a io := 'a P.io
       and module IO = P.IO
       and module Merge = P.Merge
       and type key = P.Node.Path.t
       and type contents = P.Contents.value
       and type branch = P.Branch.key
       and type hash = P.Hash.t
       and type slice = P.Slice.t
       and type step = P.Node.Path.step
       and type metadata = P.Node.Metadata.t
       and module Key = P.Node.Path
       and type repo = P.Repo.t
       and module Private = P

  module Json_tree : JSON_TREE
  (** [Json_tree] is used to project JSON values onto trees. Instead of the
      entire object being stored under one key, it is split across several keys
      starting at the specified root key. *)

  module Content_addressable
      (IO : IO.S)
      (X : Sigs.APPEND_ONLY_STORE_MAKER with type 'a io := 'a IO.t) : sig
    type 'a io = 'a IO.t

    module Make : functor (K : Hash.S) (V : Type.S) ->
      Sigs.CONTENT_ADDRESSABLE_STORE_EXT
        with type key = K.t
         and type value = V.t
         and type 'a t = 'a X.Make(K)(V).t
         and type 'a io := 'a io
  end

  module Of_direct : sig
    module Append_only
        (DST : IO.S)
        (X : Sigs.APPEND_ONLY_STORE_MAKER with type 'a io := 'a) :
      Sigs.APPEND_ONLY_STORE_MAKER with type 'a io = 'a DST.t

    module Content_addressable
        (DST : IO.S)
        (X : Sigs.CONTENT_ADDRESSABLE_STORE_MAKER with type 'a io := 'a) :
      Sigs.CONTENT_ADDRESSABLE_STORE_MAKER with type 'a io = 'a DST.t
  end
end
