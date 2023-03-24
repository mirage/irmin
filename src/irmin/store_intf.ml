(*
 * Copyright (c) 2013-2022 Thomas Gazagnaire <thomas@gazagnaire.org>
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
open Store_properties

module type S_generic_key = sig
  (** {1 Irmin stores}

      Irmin stores are tree-like read-write stores with extended capabilities.
      They allow an application (or a collection of applications) to work with
      multiple local states, which can be forked and merged programmatically,
      without having to rely on a global state. In a way very similar to version
      control systems, Irmin local states are called {i branches}.

      There are two kinds of store in Irmin: the ones based on {{!of_branch}
      persistent} named branches and the ones based {{!of_commit} temporary}
      detached heads. These exist relative to a local, larger (and shared)
      store, and have some (shared) contents. This is exactly the same as usual
      version control systems, that the informed user can see as an implicit
      purely functional data-structure. *)

  module Schema : Schema.S

  type repo
  (** The type for Irmin repositories. *)

  type t
  (** The type for Irmin stores. *)

  type step = Schema.Path.step [@@deriving irmin]
  (** The type for {!type-key} steps. *)

  type path = Schema.Path.t [@@deriving irmin]
  (** The type for store keys. A key is a sequence of {!step}s. *)

  type metadata = Schema.Metadata.t [@@deriving irmin]
  (** The type for store metadata. *)

  type contents = Schema.Contents.t [@@deriving irmin]
  (** The type for store contents. *)

  type node [@@deriving irmin]
  (** The type for store nodes. *)

  type tree [@@deriving irmin]
  (** The type for store trees. *)

  type hash = Schema.Hash.t [@@deriving irmin]
  (** The type for object hashes. *)

  type commit
  (** Type for [`Commit] identifiers. Similar to Git's commit SHA1s. *)

  val commit_t : repo -> commit Type.t
  (** [commit_t r] is the value type for {!commit}. *)

  type branch = Schema.Branch.t [@@deriving irmin]
  (** Type for persistent branch names. Branches usually share a common global
      namespace and it's the user's responsibility to avoid name clashes. *)

  type slice [@@deriving irmin]
  (** Type for store slices. *)

  type info = Schema.Info.t [@@deriving irmin]
  (** The type for commit info. *)

  type lca_error = [ `Max_depth_reached | `Too_many_lcas ] [@@deriving irmin]
  (** The type for errors associated with functions computing least common
      ancestors *)

  type ff_error = [ `No_change | `Rejected | lca_error ] [@@deriving irmin]
  (** The type for errors for {!Head.fast_forward}. *)

  module Info : sig
    include Info.S with type t = info
    (** @inline *)
  end

  type contents_key [@@deriving irmin]
  type node_key [@@deriving irmin]
  type commit_key [@@deriving irmin]

  (** Repositories. *)
  module Repo : sig
    (** {1 Repositories}

        A repository contains a set of branches. *)

    type t = repo
    (** The type of repository handles. *)

    val v : Conf.t -> t Lwt.t
    (** [v config] connects to a repository in a backend-specific manner. *)

    val config : t -> Conf.t
    (** [config repo] is the configuration used to create [repo] *)

    include Closeable with type _ t := t
    (** @inline *)

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

    type elt =
      [ `Commit of commit_key
      | `Node of node_key
      | `Contents of contents_key
      | `Branch of branch ]
    [@@deriving irmin]
    (** The type for elements iterated over by {!iter}. *)

    val default_pred_commit : t -> commit_key -> elt list Lwt.t
    val default_pred_node : t -> node_key -> elt list Lwt.t
    val default_pred_contents : t -> contents_key -> elt list Lwt.t

    val iter :
      ?cache_size:int ->
      min:elt list ->
      max:elt list ->
      ?edge:(elt -> elt -> unit Lwt.t) ->
      ?branch:(branch -> unit Lwt.t) ->
      ?commit:(commit_key -> unit Lwt.t) ->
      ?node:(node_key -> unit Lwt.t) ->
      ?contents:(contents_key -> unit Lwt.t) ->
      ?skip_branch:(branch -> bool Lwt.t) ->
      ?skip_commit:(commit_key -> bool Lwt.t) ->
      ?skip_node:(node_key -> bool Lwt.t) ->
      ?skip_contents:(contents_key -> bool Lwt.t) ->
      ?pred_branch:(t -> branch -> elt list Lwt.t) ->
      ?pred_commit:(t -> commit_key -> elt list Lwt.t) ->
      ?pred_node:(t -> node_key -> elt list Lwt.t) ->
      ?pred_contents:(t -> contents_key -> elt list Lwt.t) ->
      ?rev:bool ->
      t ->
      unit Lwt.t
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

    val breadth_first_traversal :
      ?cache_size:int ->
      max:elt list ->
      ?branch:(branch -> unit Lwt.t) ->
      ?commit:(commit_key -> unit Lwt.t) ->
      ?node:(node_key -> unit Lwt.t) ->
      ?contents:(contents_key -> unit Lwt.t) ->
      ?pred_branch:(t -> branch -> elt list Lwt.t) ->
      ?pred_commit:(t -> commit_key -> elt list Lwt.t) ->
      ?pred_node:(t -> node_key -> elt list Lwt.t) ->
      ?pred_contents:(t -> contents_key -> elt list Lwt.t) ->
      t ->
      unit Lwt.t
  end

  val empty : repo -> t Lwt.t
  (** [empty repo] is a temporary, empty store. Becomes a normal temporary store
      after the first update. *)

  val main : repo -> t Lwt.t
  (** [main r] is a persistent store based on [r]'s main branch. This operation
      is cheap, can be repeated multiple times. *)

  val of_branch : repo -> branch -> t Lwt.t
  (** [of_branch r name] is a persistent store based on the branch [name].
      Similar to {!main}, but use [name] instead of {!Irmin.Branch.S.main}. *)

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
    (** [t] is the value type for {!type-t}. *)

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
    (** [fast_forward t h] is similar to {!set} but the [t]'s head is updated to
        [h] only if [h] is stricly in the future of [t]'s current head.
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
    (** Same as {!set} but check that the value is [test] before updating to
        [set]. Use {!set} or {!val-merge} instead if possible. *)

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
    (** [t] is the value type for {!type-t}. *)

    val pp_hash : t Fmt.t
    (** [pp] is the pretty-printer for commit. Display only the hash. *)

    val v :
      ?clear:bool ->
      repo ->
      info:info ->
      parents:commit_key list ->
      tree ->
      commit Lwt.t
    (** [v r i ~parents:p t] is the commit [c] such that:

        - [info c = i]
        - [parents c = p]
        - [tree c = t]

        When [clear] is set (the default), the tree cache is emptied upon the
        function's completion, mirroring the effect of invoking {!Tree.clear}. *)

    val tree : commit -> tree
    (** [tree c] is [c]'s root tree. *)

    val parents : commit -> commit_key list
    (** [parents c] are [c]'s parents. *)

    val info : commit -> info
    (** [info c] is [c]'s info. *)

    val hash : commit -> hash
    (** [hash c] is [c]'s hash. *)

    (** {1 Import/Export} *)

    val key : commit -> commit_key
    (** [key c] is [c]'s key. *)

    val of_key : repo -> commit_key -> commit option Lwt.t
    (** [of_key r k] is the the commit object in [r] with key [k], or [None] if
        no such commit object exists. *)

    val of_hash : repo -> hash -> commit option Lwt.t
    (** [of_hash r h] is the commit object in [r] with hash [h], or [None] if no
        such commit object is indexed in [r].

        {b Note:} in stores for which {!commit_key} = {!type-hash}, this
        function has identical behaviour to {!of_key}. *)
  end

  (** [Contents] provides base functions for the store's contents. *)
  module Contents : sig
    include Contents.S with type t = contents

    (** {1 Import/Export} *)

    val hash : contents -> hash
    (** [hash c] it [c]'s hash. *)

    val of_key : repo -> contents_key -> contents option Lwt.t
    (** [of_key r k] is the contents object in [r] with key [k], or [None] if no
        such contents object exists. *)

    val of_hash : repo -> hash -> contents option Lwt.t
    (** [of_hash r h] is the contents object in [r] with hash [h], or [None] if
        no such contents object is indexed in [r].

        {b Note:} in stores for which {!contents_key} = {!type-hash}, this
        function has identical behaviour to {!of_key}. *)
  end

  (** Managing store's trees. *)
  module Tree : sig
    include
      Tree.S
        with type t := tree
         and type step := step
         and type path := path
         and type metadata := metadata
         and type contents := contents
         and type contents_key := contents_key
         and type node := node
         and type hash := hash

    (** {1 Import/Export} *)

    type kinded_key =
      [ `Contents of contents_key * metadata | `Node of node_key ]
    [@@deriving irmin]
    (** Keys in the Irmin store are tagged with the type of the value they
        reference (either {!contents} or {!node}). In the [contents] case, the
        key is paired with corresponding {!metadata}. *)

    val key : tree -> kinded_key option
    (** [key t] is the key of tree [t] in the underlying repository, if it
        exists. Tree objects that exist entirely in memory (such as those built
        with {!of_concrete}) have no backend key until they are exported to a
        repository, and so will return [None]. *)

    val find_key : Repo.t -> tree -> kinded_key option Lwt.t
    (** [find_key r t] is the key of a tree object with the same hash as [t] in
        [r], if such a key exists and is indexed. *)

    val of_key : Repo.t -> kinded_key -> tree option Lwt.t
    (** [of_key r h] is the tree object in [r] having [h] as key, or [None] if
        no such tree object exists. *)

    val shallow : Repo.t -> kinded_key -> tree
    (** [shallow r h] is the shallow tree object with the key [h]. No check is
        performed to verify if [h] actually exists in [r]. *)

    val hash : ?cache:bool -> tree -> hash
    (** [hash t] is the hash of tree [t]. *)

    type kinded_hash = [ `Contents of hash * metadata | `Node of hash ]
    (** Like {!kinded_key}, but with hashes as value references rather than
        keys. *)

    val kinded_hash : ?cache:bool -> tree -> kinded_hash
    (** [kinded_hash t] is [c]'s kinded hash. *)

    val of_hash : Repo.t -> kinded_hash -> tree option Lwt.t
    (** [of_hash r h] is the tree object in [r] with hash [h], or [None] if no
        such tree object is indexed in [r].

        {b Note:} in stores for which {!node_key} = {!contents_key} =
        {!type-hash}, this function has identical behaviour to {!of_key}. *)

    (** {1 Proofs} *)

    type ('proof, 'result) producer :=
      repo ->
      kinded_key ->
      (tree -> (tree * 'result) Lwt.t) ->
      ('proof * 'result) Lwt.t
    (** [produce r h f] runs [f] on top of a real store [r], producing a proof
        and a result using the initial root hash [h].

        The trees produced during [f]'s computation will carry the full history
        of reads. This history will be reset when [f] is complete so subtrees
        escaping the scope of [f] will not cause memory leaks.

        Calling [produce_proof] recursively has an undefined behaviour. *)

    type verifier_error =
      [ `Proof_mismatch of string
      | `Stream_too_long of string
      | `Stream_too_short of string ]
    [@@deriving irmin]
    (** The type for errors associated with functions that verify proofs. *)

    type ('proof, 'result) verifier :=
      'proof ->
      (tree -> (tree * 'result) Lwt.t) ->
      (tree * 'result, verifier_error) result Lwt.t
    (** [verify p f] runs [f] in checking mode. [f] is a function that takes a
        tree as input and returns a new version of the tree and a result. [p] is
        a proof, that is a minimal representation of the tree that contains what
        [f] should be expecting.

        Therefore, contrary to trees found in a storage, the contents of the
        trees passed to [f] may not be available. For this reason, looking up a
        value at some [path] can now produce three distinct outcomes:

        - A value [v] is present in the proof [p] and returned :
          [find tree path] is a promise returning [Some v];
        - [path] is known to have no value in [tree] : [find tree path] is a
          promise returning [None]; and
        - [path] is known to have a value in [tree] but [p] does not provide it
          because [f] should not need it: [verify] returns an error classifying
          [path] as an invalid path (see below).

        The same semantics apply to all operations on the tree [t] passed to [f]
        and on all operations on the trees built from [f].

        The generated tree is the tree after [f] has completed. That tree is
        disconnected from the backend. It is possible to run operations on it as
        long as they don't require loading shallowed subtrees, otherwise it
        would raise [Dangling_hash].

        The result is [Error _] if the proof is rejected:

        - For tree proofs: when [p.before] is different from the hash of
          [p.state];
        - For tree and stream proofs: when [p.after] is different from the hash
          of [f p.state];
        - For tree and stream proofs: when [f p.state] tries to access paths
          invalid paths in [p.state];
        - For stream proofs: when the proof is not empty once [f] is done. *)

    type tree_proof := Proof.tree Proof.t
    (** The type for tree proofs.

        Guarantee that the given computation performs exactly the same state
        operations as the generating computation, *in some order*. *)

    val produce_proof : (tree_proof, 'a) producer
    (** [produce_proof] is the producer of tree proofs. *)

    val verify_proof : (tree_proof, 'a) verifier
    (** [verify_proof] is the verifier of tree proofs. *)

    val hash_of_proof_state : Proof.tree -> kinded_hash

    type stream_proof := Proof.stream Proof.t
    (** The type for stream proofs.

        Guarantee that the given computation performs exactly the same state
        operations as the generating computation, in the exact same order.

        Calling [fold] with [order = `Undefined] during the
        production/verification of streamed proofs is undefined. *)

    val produce_stream : (stream_proof, 'a) producer
    (** [produce_stream] is the producer of stream proofs. *)

    val verify_stream : (stream_proof, 'a) verifier
    (** [verify_stream] is the verifier of stream proofs. *)
  end

  (** {1 Reads} *)

  val kind : t -> path -> [ `Contents | `Node ] option Lwt.t
  (** [kind] is {!Tree.kind} applied to [t]'s root tree. *)

  val list : t -> path -> (step * tree) list Lwt.t
  (** [list t] is {!Tree.list} applied to [t]'s root tree. *)

  val mem : t -> path -> bool Lwt.t
  (** [mem t] is {!Tree.mem} applied to [t]'s root tree. *)

  val mem_tree : t -> path -> bool Lwt.t
  (** [mem_tree t] is {!Tree.mem_tree} applied to [t]'s root tree. *)

  val find_all : t -> path -> (contents * metadata) option Lwt.t
  (** [find_all t] is {!Tree.find_all} applied to [t]'s root tree. *)

  val find : t -> path -> contents option Lwt.t
  (** [find t] is {!Tree.find} applied to [t]'s root tree. *)

  val get_all : t -> path -> (contents * metadata) Lwt.t
  (** [get_all t] is {!Tree.get_all} applied on [t]'s root tree. *)

  val get : t -> path -> contents Lwt.t
  (** [get t] is {!Tree.get} applied to [t]'s root tree. *)

  val find_tree : t -> path -> tree option Lwt.t
  (** [find_tree t] is {!Tree.find_tree} applied to [t]'s root tree. *)

  val get_tree : t -> path -> tree Lwt.t
  (** [get_tree t k] is {!Tree.get_tree} applied to [t]'s root tree. *)

  type kinded_key := [ `Contents of contents_key | `Node of node_key ]

  val key : t -> path -> kinded_key option Lwt.t
  (** [id t k] *)

  val hash : t -> path -> hash option Lwt.t
  (** [hash t k] *)

  (** {1 Updates} *)

  type write_error =
    [ Merge.conflict | `Too_many_retries of int | `Test_was of tree option ]
  [@@deriving irmin]
  (** The type for write errors.

      - Merge conflict.
      - Concurrent transactions are competing to get the current operation
        committed and too many attemps have been tried (livelock).
      - A "test and set" operation has failed and the current value is [v]
        instead of the one we were waiting for. *)

  val set :
    ?clear:bool ->
    ?retries:int ->
    ?allow_empty:bool ->
    ?parents:commit list ->
    info:Info.f ->
    t ->
    path ->
    contents ->
    (unit, write_error) result Lwt.t
  (** [set t k ~info v] sets [k] to the value [v] in [t]. Discard any previous
      results but ensure that no operation is lost in the history.

      This function always uses {!Metadata.default} as metadata. Use {!set_tree}
      with `[Contents (c, m)] for different ones.

      When [clear] is set (the default), the tree cache is emptied upon the
      function's completion, mirroring the effect of invoking {!Tree.clear}.

      The result is [Error `Too_many_retries] if the concurrent operations do
      not allow the operation to commit to the underlying storage layer
      (livelock). *)

  val set_exn :
    ?clear:bool ->
    ?retries:int ->
    ?allow_empty:bool ->
    ?parents:commit list ->
    info:Info.f ->
    t ->
    path ->
    contents ->
    unit Lwt.t
  (** [set_exn] is like {!set} but raise [Failure _] instead of using a result
      type. *)

  val set_tree :
    ?clear:bool ->
    ?retries:int ->
    ?allow_empty:bool ->
    ?parents:commit list ->
    info:Info.f ->
    t ->
    path ->
    tree ->
    (unit, write_error) result Lwt.t
  (** [set_tree] is like {!set} but for trees. *)

  val set_tree_exn :
    ?clear:bool ->
    ?retries:int ->
    ?allow_empty:bool ->
    ?parents:commit list ->
    info:Info.f ->
    t ->
    path ->
    tree ->
    unit Lwt.t
  (** [set_tree] is like {!set_exn} but for trees. *)

  val remove :
    ?clear:bool ->
    ?retries:int ->
    ?allow_empty:bool ->
    ?parents:commit list ->
    info:Info.f ->
    t ->
    path ->
    (unit, write_error) result Lwt.t
  (** [remove t ~info k] remove any bindings to [k] in [t].

      The result is [Error `Too_many_retries] if the concurrent operations do
      not allow the operation to commit to the underlying storage layer
      (livelock). *)

  val remove_exn :
    ?clear:bool ->
    ?retries:int ->
    ?allow_empty:bool ->
    ?parents:commit list ->
    info:Info.f ->
    t ->
    path ->
    unit Lwt.t
  (** [remove_exn] is like {!remove} but raise [Failure _] instead of a using
      result type. *)

  val test_and_set :
    ?clear:bool ->
    ?retries:int ->
    ?allow_empty:bool ->
    ?parents:commit list ->
    info:Info.f ->
    t ->
    path ->
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
    ?clear:bool ->
    ?retries:int ->
    ?allow_empty:bool ->
    ?parents:commit list ->
    info:Info.f ->
    t ->
    path ->
    test:contents option ->
    set:contents option ->
    unit Lwt.t
  (** [test_and_set_exn] is like {!test_and_set} but raise [Failure _] instead
      of using a result type. *)

  val test_and_set_tree :
    ?clear:bool ->
    ?retries:int ->
    ?allow_empty:bool ->
    ?parents:commit list ->
    info:Info.f ->
    t ->
    path ->
    test:tree option ->
    set:tree option ->
    (unit, write_error) result Lwt.t
  (** [test_and_set_tree] is like {!test_and_set} but for trees. *)

  val test_and_set_tree_exn :
    ?clear:bool ->
    ?retries:int ->
    ?allow_empty:bool ->
    ?parents:commit list ->
    info:Info.f ->
    t ->
    path ->
    test:tree option ->
    set:tree option ->
    unit Lwt.t
  (** [test_and_set_tree_exn] is like {!test_and_set_exn} but for trees. *)

  val test_set_and_get :
    ?clear:bool ->
    ?retries:int ->
    ?allow_empty:bool ->
    ?parents:commit list ->
    info:(unit -> info) ->
    t ->
    path ->
    test:contents option ->
    set:contents option ->
    (commit option, write_error) result Lwt.t
  (** [test_set_and_get] is like {!test_and_set} except it also returns the
      commit associated with updating the store with the new value if the
      [test_and_set] is successful. No commit is returned if there was no update
      to the store. *)

  val test_set_and_get_exn :
    ?clear:bool ->
    ?retries:int ->
    ?allow_empty:bool ->
    ?parents:commit list ->
    info:(unit -> info) ->
    t ->
    path ->
    test:contents option ->
    set:contents option ->
    commit option Lwt.t
  (** [test_set_and_get_exn] is like {!test_set_and_get} but raises [Failure _]
      instead. *)

  val test_set_and_get_tree :
    ?clear:bool ->
    ?retries:int ->
    ?allow_empty:bool ->
    ?parents:commit list ->
    info:(unit -> info) ->
    t ->
    path ->
    test:tree option ->
    set:tree option ->
    (commit option, write_error) result Lwt.t
  (** [test_set_and_get_tree] is like {!test_set_and_get} but for a {!tree} *)

  val test_set_and_get_tree_exn :
    ?clear:bool ->
    ?retries:int ->
    ?allow_empty:bool ->
    ?parents:commit list ->
    info:(unit -> info) ->
    t ->
    path ->
    test:tree option ->
    set:tree option ->
    commit option Lwt.t
  (** [test_set_and_get_tree_exn] is like {!test_set_and_get_tree} but raises
      [Failure _] instead. *)

  val merge :
    ?clear:bool ->
    ?retries:int ->
    ?allow_empty:bool ->
    ?parents:commit list ->
    info:Info.f ->
    old:contents option ->
    t ->
    path ->
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
    ?clear:bool ->
    ?retries:int ->
    ?allow_empty:bool ->
    ?parents:commit list ->
    info:Info.f ->
    old:contents option ->
    t ->
    path ->
    contents option ->
    unit Lwt.t
  (** [merge_exn] is like {!val-merge} but raise [Failure _] instead of using a
      result type. *)

  val merge_tree :
    ?clear:bool ->
    ?retries:int ->
    ?allow_empty:bool ->
    ?parents:commit list ->
    info:Info.f ->
    old:tree option ->
    t ->
    path ->
    tree option ->
    (unit, write_error) result Lwt.t
  (** [merge_tree] is like {!merge_tree} but for trees. *)

  val merge_tree_exn :
    ?clear:bool ->
    ?retries:int ->
    ?allow_empty:bool ->
    ?parents:commit list ->
    info:Info.f ->
    old:tree option ->
    t ->
    path ->
    tree option ->
    unit Lwt.t
  (** [merge_tree] is like {!merge_tree} but for trees. *)

  val with_tree :
    ?clear:bool ->
    ?retries:int ->
    ?allow_empty:bool ->
    ?parents:commit list ->
    ?strategy:[ `Set | `Test_and_set | `Merge ] ->
    info:Info.f ->
    t ->
    path ->
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
      - if [strategy = `Merge], use {!val-merge} and ensure that concurrent
        updates and merged with the values present at the beginning of the
        transaction.

      {b Note:} Irmin transactions provides
      {{:https://en.wikipedia.org/wiki/Snapshot_isolation} snapshot isolation}
      guarantees: reads and writes are isolated in every transaction, but only
      write conflicts are visible on commit. *)

  val with_tree_exn :
    ?clear:bool ->
    ?retries:int ->
    ?allow_empty:bool ->
    ?parents:commit list ->
    ?strategy:[ `Set | `Test_and_set | `Merge ] ->
    info:Info.f ->
    t ->
    path ->
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

  val watch : t -> ?init:commit -> (commit Diff.t -> unit Lwt.t) -> watch Lwt.t
  (** [watch t f] calls [f] every time the contents of [t]'s head is updated.

      {b Note:} even if [f] might skip some head updates, it will never be
      called concurrently: all consecutive calls to [f] are done in sequence, so
      we ensure that the previous one ended before calling the next one. *)

  val watch_key :
    t ->
    path ->
    ?init:commit ->
    ((commit * tree) Diff.t -> unit Lwt.t) ->
    watch Lwt.t
  (** [watch_key t key f] calls [f] every time the [key]'s value is added,
      removed or updated. If the current branch is deleted, no signal is sent to
      the watcher. *)

  val unwatch : watch -> unit Lwt.t
  (** [unwatch w] disable [w]. Return once the [w] is fully disabled. *)

  (** {1 Merges and Common Ancestors} *)

  type 'a merge =
    info:Info.f ->
    ?max_depth:int ->
    ?n:int ->
    'a ->
    (unit, Merge.conflict) result Lwt.t
  (** The type for merge functions. *)

  val merge_into : into:t -> t merge
  (** [merge_into ~into:x ~info:i t] merges [t]'s current branch into [x]'s
      current branch using the info [i]. After that operation, the two stores
      are still independent. Similar to [git merge <branch>]. *)

  val merge_with_branch : t -> branch merge
  (** Same as {!val-merge} but with a branch ID. *)

  val merge_with_commit : t -> commit merge
  (** Same as {!val-merge} but with a commit_id. *)

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
  (** Same as {!lcas} but takes a commmit as argument. *)

  (** {1 History} *)

  module History : Graph.Sig.P with type V.t = commit
  (** An history is a DAG of heads. *)

  val history :
    ?depth:int -> ?min:commit list -> ?max:commit list -> t -> History.t Lwt.t
  (** [history ?depth ?min ?max t] is a view of the history of the store [t], of
      depth at most [depth], starting from the [t]'s head (or from [max] if the
      head is not set) and stopping at [min] if specified. *)

  val last_modified : ?depth:int -> ?n:int -> t -> path -> commit list Lwt.t
  (** [last_modified ?number c k] is the list of the last [number] commits that
      modified [path], in ascending order of date. [depth] is the maximum depth
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
      (commit Diff.t -> unit Lwt.t) ->
      watch Lwt.t
    (** [watch t b f] calls [f] on every change in [b]. *)

    val watch_all :
      repo ->
      ?init:(branch * commit) list ->
      (branch -> commit Diff.t -> unit Lwt.t) ->
      watch Lwt.t
    (** [watch_all t f] calls [f] on every branch-related change in [t],
        including creation/deletion events. *)

    include Branch.S with type t = branch
    (** Base functions for branches. *)
  end

  (** [Path] provides base functions for the stores's paths. *)
  module Path : Path.S with type t = path and type step = step

  module Metadata : Metadata.S with type t = metadata
  (** [Metadata] provides base functions for node metadata. *)

  (** Backend functions, which might be used by the backends. *)
  module Backend :
    Backend.S
      with module Schema = Schema
      with type Slice.t = slice
       and type Repo.t = repo
       and module Hash = Hash
       and module Node.Path = Path
       and type Contents.key = contents_key
       and type Node.key = node_key
       and type Commit.key = commit_key

  type Remote.t +=
    | E of Backend.Remote.endpoint
          (** Extend the [remote] type with [endpoint]. *)

  (** {2 Converters to backend types} *)

  val of_backend_node : repo -> Backend.Node.value -> node
  val to_backend_node : node -> Backend.Node.value Lwt.t
  val to_backend_portable_node : node -> Backend.Node_portable.t Lwt.t

  val to_backend_commit : commit -> Backend.Commit.value
  (** [to_backend_commit c] is the backend commit object associated with the
      commit [c]. *)

  val of_backend_commit :
    repo -> Backend.Commit.Key.t -> Backend.Commit.value -> commit
  (** [of_backend_commit r k c] is the commit associated with the backend commit
      object [c] that hash key [k] in [r]. *)

  val save_contents :
    [> write ] Backend.Contents.t -> contents -> contents_key Lwt.t
  (** Save a content into the database *)

  val save_tree :
    ?clear:bool ->
    repo ->
    [> write ] Backend.Contents.t ->
    [> read_write ] Backend.Node.t ->
    tree ->
    kinded_key Lwt.t
  (** Save a tree into the database. Does not do any reads.

      When [clear] is set (the default), the tree cache is emptied upon the
      function's completion, mirroring the effect of invoking {!Tree.clear}. *)

  (** {Deprecated} *)

  val master : repo -> t Lwt.t
    [@@ocaml.deprecated "Use `main` instead."]
  (** @deprecated Use {!main} instead *)
end

module type S = sig
  type hash

  (** @inline *)
  include
    S_generic_key
      with type Schema.Hash.t = hash
       and type hash := hash
       and type contents_key = hash
       and type node_key = hash
       and type commit_key = hash
end

module S_is_a_generic_keyed (X : S) : S_generic_key = X

module type Maker_generic_key = sig
  type endpoint

  include Key.Store_spec.S

  module Make (Schema : Schema.S) :
    S_generic_key
      with module Schema = Schema
       and type Backend.Remote.endpoint = endpoint
       and type contents_key = (Schema.Hash.t, Schema.Contents.t) contents_key
       and type node_key = Schema.Hash.t node_key
       and type commit_key = Schema.Hash.t commit_key
end

module type Maker =
  Maker_generic_key
    with type ('h, _) contents_key = 'h
     and type 'h node_key = 'h
     and type 'h commit_key = 'h

module type Json_tree = functor
  (Store : S with type Schema.Contents.t = Contents.json)
  -> sig
  include Contents.S with type t = Contents.json

  val to_concrete_tree : t -> Store.Tree.concrete
  val of_concrete_tree : Store.Tree.concrete -> t

  val get_tree : Store.tree -> Store.path -> t Lwt.t
  (** Extract a [json] value from tree at the given key. *)

  val set_tree : Store.tree -> Store.path -> t -> Store.tree Lwt.t
  (** Project a [json] value onto a tree at the given key. *)

  val get : Store.t -> Store.path -> t Lwt.t
  (** Extract a [json] value from a store at the given key. *)

  val set :
    Store.t -> Store.path -> t -> info:(unit -> Store.info) -> unit Lwt.t
  (** Project a [json] value onto a store at the given key. *)
end

module type KV_generic_key =
  S_generic_key
    with type Schema.Path.step = string
     and type Schema.Path.t = string list
     and type Schema.Branch.t = string

module type KV =
  S
    with type Schema.Path.step = string
     and type Schema.Path.t = string list
     and type Schema.Branch.t = string

module type KV_maker_generic_key = sig
  type endpoint
  type metadata
  type hash
  type info

  include Key.Store_spec.S

  module Make (C : Contents.S) :
    KV_generic_key
      with module Schema.Contents = C
       and type Schema.Metadata.t = metadata
       and type Backend.Remote.endpoint = endpoint
       and type Schema.Hash.t = hash
       and type contents_key = (hash, C.t) contents_key
       and type node_key = hash node_key
       and type commit_key = hash commit_key
       and type Schema.Info.t = info
end

module type KV_maker =
  KV_maker_generic_key
    with type ('h, _) contents_key = 'h
     and type 'h node_key = 'h
     and type 'h commit_key = 'h

module type Sigs = sig
  module type S = S
  module type Maker = Maker
  module type Json_tree = Json_tree
  module type KV = KV
  module type KV_maker = KV_maker

  module Generic_key : sig
    module type S = S_generic_key
    module type KV = KV_generic_key
    module type Maker = Maker_generic_key
    module type KV_maker = KV_maker_generic_key
  end

  type Remote.t +=
    | Store : (module Generic_key.S with type t = 'a) * 'a -> Remote.t

  module Make (B : Backend.S) :
    Generic_key.S
      with module Schema = B.Schema
       and type slice = B.Slice.t
       and type repo = B.Repo.t
       and type contents_key = B.Contents.key
       and type node_key = B.Node.key
       and type commit_key = B.Commit.key
       and module Backend = B

  module Json_tree : Json_tree
  (** [Json_tree] is used to project JSON values onto trees. Instead of the
      entire object being stored under one key, it is split across several keys
      starting at the specified root key. *)
end
