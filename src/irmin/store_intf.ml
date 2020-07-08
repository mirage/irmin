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

module Sigs = S

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

  (** Repositories. *)
  module Repo : sig
    (** {1 Repositories}

        A repository contains a set of branches. *)

    type t = repo
    (** The type of repository handles. *)

    val v : S.config -> t Lwt.t
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

  module Hash : S.HASH with type t = hash
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
    include S.CONTENTS with type t = contents

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
      Tree.S
        with type t := tree
         and type step := step
         and type key := key
         and type metadata := metadata
         and type contents := contents
         and type node := node
         and type hash := hash

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

  val watch : t -> ?init:commit -> (commit S.diff -> unit Lwt.t) -> watch Lwt.t
  (** [watch t f] calls [f] every time the contents of [t]'s head is updated.

      {b Note:} even if [f] might skip some head updates, it will never be
      called concurrently: all consecutive calls to [f] are done in sequence, so
      we ensure that the previous one ended before calling the next one. *)

  val watch_key :
    t ->
    key ->
    ?init:commit ->
    ((commit * tree) S.diff -> unit Lwt.t) ->
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
      (commit S.diff -> unit Lwt.t) ->
      watch Lwt.t
    (** [watch t b f] calls [f] on every change in [b]. *)

    val watch_all :
      repo ->
      ?init:(branch * commit) list ->
      (branch -> commit S.diff -> unit Lwt.t) ->
      watch Lwt.t
    (** [watch_all t f] calls [f] on every branch-related change in [t],
        including creation/deletion events. *)

    include S.BRANCH with type t = branch
    (** Base functions for branches. *)
  end

  (** [Key] provides base functions for the stores's paths. *)
  module Key : S.PATH with type t = key and type step = step

  module Metadata : S.METADATA with type t = metadata
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
      S.PRIVATE
        with type Contents.value = contents
         and module Hash = Hash
         and module Node.Path = Key
         and type Node.Metadata.t = metadata
         and type Branch.key = branch
         and type Slice.t = slice
         and type Repo.t = repo
  end

  type S.remote +=
    | E of Private.Sync.endpoint
          (** Extend the [remote] type with [endpoint]. *)

  (** {2 Converters to private types} *)

  val to_private_node : node -> Private.Node.value Tree.or_error Lwt.t

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
  (M : S.METADATA)
  (C : S.CONTENTS)
  (P : S.PATH)
  (B : S.BRANCH)
  (H : S.HASH)
  ->
  S
    with type key = P.t
     and type step = P.step
     and type metadata = M.t
     and type contents = C.t
     and type branch = B.t
     and type hash = H.t
     and type Private.Sync.endpoint = unit

module type Store = sig
  module type S = S

  module type MAKER = MAKER

  type Sigs.remote += Store : (module S with type t = 'a) * 'a -> Sigs.remote

  module Make (P : Sigs.PRIVATE) :
    S
      with type key = P.Node.Path.t
       and type contents = P.Contents.value
       and type branch = P.Branch.key
       and type hash = P.Hash.t
       and type slice = P.Slice.t
       and type step = P.Node.Path.step
       and type metadata = P.Node.Val.metadata
       and module Key = P.Node.Path
       and type repo = P.Repo.t
       and module Private = P

  module Content_addressable
      (X : Sigs.APPEND_ONLY_STORE_MAKER)
      (K : Sigs.HASH)
      (V : Type.S) : sig
    include
      Sigs.CONTENT_ADDRESSABLE_STORE
        with type 'a t = 'a X(K)(V).t
         and type key = K.t
         and type value = V.t

    val batch : [ `Read ] t -> ([ `Read | `Write ] t -> 'a Lwt.t) -> 'a Lwt.t

    val v : Conf.t -> [ `Read ] t Lwt.t

    val close : 'a t -> unit Lwt.t
  end
end
