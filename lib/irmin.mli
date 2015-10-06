(*
 * Copyright (c) 2013-2015 Thomas Gazagnaire <thomas@gazagnaire.org>
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

    [Irmin] is a library to design and use persistent stores with
    built-in snapshot, branching and reverting mechanisms. Irmin uses
    concepts similar to {{:http://git-scm.com/}Git} but it exposes
    them as a high level library instead of a complex command-line
    frontend. It features a {e bidirectional} Git backend,
    fully-compatible with the usual Git tools and workflows.

    Irmin is designed to use a large variety of backends. It is
    written in pure OCaml and does not depend on external C stubs; it
    is thus very portable and aims to run everywhere, from Linux to
    Xen unikernels.

    Consult the {!basics} and {!examples} of use for a quick
    start. See also the {{!Irmin_unix}documentation} for the unix
    backends.

    {e Release %%VERSION%% - %%MAINTAINER%% }
*)

val version: string
(** The version of the library. *)

(** {1 Preliminaries} *)

(** Serializable data with reversible human-readable
    representations. *)
module Hum: sig

  (** {1 Human-representable values} *)

  module type S = sig

    include Tc.S0

    val to_hum: t -> string
    (** Display a value using its human readable representation. *)

    val of_hum: string -> t
    (** Convert a human readable representation of a value into its
        abstract value.

        @raise Invalid_argument if the string does not represent
        anything meaningful. *)

  end

  type 'a t = (module S with type t = 'a)
  (** Type for implementation of [S] for values of type ['a]. *)

end

(** Tasks are used to keep track of the origin of reads and writes in
    the store. Every high-level operation is expected to have its own
    task which is passed to every low-level call. *)
module Task: sig

  (** {1 Task} *)

  include Tc.S0

  val create: date:int64 -> owner:string -> ?uid:int64 -> string -> t
  (** Create a new task. *)

  val date: t -> int64
  (** Get the task date.

      The date provided by the user when calling the
      {{!Task.create}create} function.  Rounding [Unix.gettimeofday ()]
      (when available) is a good value for such date.  On more
      esoteric platforms, any monotonic counter is a fine value as
      well.  On the Git backend, the date is translated into the
      commit {e Date} field and is expected to be the number of
      POSIX seconds (thus not counting leap seconds) since the Epoch. *)

  val owner: t -> string
  (** Get the task owner.

      The owner identifies the entity (human, unikernel, process,
      thread, etc) performing an operation. For the Git backend, this
      will be directly translated into the {e Author} field. *)

  val uid: t -> int64
  (** Get the task unique identifier.

      By default, it is freshly generated on each call to
      {{!Task.create}create}. The identifier is useful for debugging
      purposes, for instance to relate debug lines to the tasks which
      cause them, and might appear in one line of the commit message
      for the Git backend. *)

  val messages: t -> string list
  (** Get the messages associated to the task.

      Text messages can be added to a task either at creation time,
      using {{!Task.create}create}, or can be appended on already
      created tasks using the {{!Task.add}add} function. For
      the Git backend, this will be translated to the commit
      message.  *)

  val add: t -> string -> unit
  (** Add a message to the task messages list. See
      {{!Task.messages}messages} for more details. *)

  val empty: t
  (** The empty task. *)

  (** {1 Task creators} *)

  type 'a f = 'a -> t
  (** The type for user-defined task creators. *)

  val none: unit f
  (** The empty task creator. *)

end

(** [Merge] provides functions to build custom 3-way merge operators
    for various user-defined contents. *)
module Merge: sig

  (** {1 Merge Results} *)

  type 'a result = [ `Ok of 'a | `Conflict of string ]
  (** Type for merge results. *)

  module Result: Tc.S1 with type 'a t = 'a result
  (** Base functions on results. *)

  val bind: 'a result Lwt.t -> ('a -> 'b result Lwt.t) -> 'b result Lwt.t
  (** Monadic bind for results. *)

  exception Conflict of string
  (** Exception which might be raised when merging.  *)

  val exn: 'a result -> 'a Lwt.t
  (** Convert [`Conflict] results to [Conflict] exceptions. *)

  (** {1 Merge Combinators} *)

  type 'a promise = unit -> 'a option result Lwt.t
  (** An ['a] promise is a function which, when called, will
      eventually return a value type of ['a]. A promise is an
      optional, lazy and non-blocking value. *)

  val promise: 'a -> 'a promise
  (** [promise a] is the promise containing [a]. *)

  val promise_map: ('a -> 'b) -> 'a promise -> 'b promise
  (** [promise_map f a] is the promise containing [f] applied to what
      is promised by [a]. *)

  val promise_bind: 'a promise -> ('a -> 'b promise) -> 'b promise
  (** [promise_bind a f] is the promise returned by [f] applied to
      what is promised by [a]. *)

  type 'a t = old:'a promise -> 'a -> 'a -> 'a result Lwt.t
  (** Signature of a merge function. [old] is the value of the
      least-common ancestor.

      {v
              /----> t1 ----\
      ----> old              |--> result
              \----> t2 ----/
      v}
  *)

  val seq: 'a t list -> 'a t
  (** Call the merge functions in sequence. Stop as soon as one is {e
      not} returning a conflict. *)

  val apply: ('a -> 'b t) -> 'a -> 'b t
  (** The [apply] combinator is useful to untie recursive loops when
      building a complex merge function. *)

  val biject: 'a Tc.t -> 'b t -> ('a -> 'b) -> ('b -> 'a) -> 'a t
  (** Use the merge function defined in another domain. If the
      converting functions raise any exception the merge is a
      conflict. *)

  val biject': 'a Tc.t -> 'b t -> ('a -> 'b Lwt.t) -> ('b -> 'a Lwt.t) -> 'a t
  (** Same as {{!Merge.biject}biject} but with blocking domain
      converting functions. *)

  (** {1 Basic Merges} *)

  val default: 'a Tc.t -> 'a t
  (** Create a default merge function. This is a simple merge
      function which supports changes in one branch at a time:

      {ul
        {- if [t1=t2] then the result of the merge is [`OK t1];}
        {- if [t1=old] then the result of the merge is [`OK t2];}
        {- if [t2=old] then return [`OK t1];}
        {- otherwise the result is [`Conflict].}
      }
  *)

  val string: string t
  (** The default string merge function. Do not do anything clever, just
      compare the strings using the [default] merge function. *)

  val option: 'a Tc.t -> 'a t -> 'a option t
  (** Lift a merge function to optional values of the same type. If all
      the provided values are inhabited, then call the provided merge
      function, otherwise use the same behavior as {!default}. *)

  val pair: 'a Tc.t -> 'b Tc.t -> 'a t -> 'b t -> ('a * 'b) t
  (** Lift merge functions to pair of elements. *)

  val triple: 'a Tc.t -> 'b Tc.t -> 'c Tc.t -> 'a t -> 'b t -> 'c t ->
    ('a * 'b * 'c) t
  (** Lift merge functions to triple of elements. *)

  val set: (module Set.S with type t = 'a) -> 'a t
  (** List merge functions to sets. *)

  (** {1 Counters and Multisets} *)

  type counter = int
  (** The type for counter values. It is expected that the only valid
      operations on counters are {e increment} and {e decrement}. The
      following merge functions ensure that the counter semantics are
      preserved: {e i.e.} it ensures that the number of increments and
      decrements is preserved. *)

  val counter: int t
  (** The merge function for mergeable counters. *)

  (** Multi-sets. *)
  module MSet (M: Map.S): sig
    val merge: counter M.t t
  end

  (** {1 Maps and Association Lists} *)

  (** We consider the only valid operations for maps and
      association lists to be:

      {ul
      {- Adding a new bindings to the map.}
      {- Removing a binding from the map.}
      {- Replacing an existing binding with a different value.}
      {- {e Trying to add an already existing binding is a no-op}.}
      }

      We thus assume that no operation on maps is modifying the {e
      key} names. So the following merge functions ensures that {e
      (i)} new bindings are preserved {e (ii)} removed bindings stay
      removed and {e (iii)} modified bindings are merged using the
      merge function of values.

      {b Note:} We only consider sets of bindings, instead of
      multisets. Application developers should take care of concurrent
      addition and removal of similar bindings themselves, by using the
      appropriate {{!Merge.MSet}multi-sets}. *)

  val alist: 'a Tc.t -> 'b Tc.t -> ('a -> 'b option t) -> ('a * 'b) list t
  (** Lift the merge functions to association lists. *)

  (** Lift the merge functions to maps. *)
  module Map (M: Map.S) (X: Tc.S0 with type t = M.key): sig

    (** {1 Merging Maps} *)

    val merge: 'a Tc.t -> (M.key -> 'a option t) -> 'a M.t t
    (** Lift to [X.t] maps. *)

  end

  (** Useful merge operators.

      [open Irmin.Merge.OP] at the top of your file to use them. *)
  module OP: sig

    (** {1 Useful operators} *)

    val ok: 'a -> 'a result Lwt.t
    (** Return [`Ok x]. *)

    val conflict: ('a, unit, string, 'b result Lwt.t) format4 -> 'a
    (** Return [`Conflict str]. *)

    val (>>|): 'a result Lwt.t -> ('a -> 'b result Lwt.t) -> 'b result Lwt.t
    (** Same as {!bind}. *)

    val (>?|): 'a promise -> ('a -> 'b promise) -> 'b promise
    (** Same as {!promise_bind}. *)

  end

end

(** {1 Stores} *)

type task = Task.t
(** The type for user-defined tasks. See {{!Task}Task}. *)

type config
(** The type for backend-specific configuration values.

    Every backend has different configuration options, which are kept
    abstract to the user. *)

type 'a diff = [`Updated of 'a * 'a | `Removed of 'a | `Added of 'a]
(** The type for representing differences betwen values. *)

(** An Irmin store is automatically built from a number of lower-level
    stores, implementing fewer operations, such as {{!AO}append-only}
    and {{!RW}read-write} stores. These low-level stores are provided
    by various backends. *)

(** Read-only stores. *)
module type RO = sig

  (** {1 Read-only stores} *)

  type t
  (** Type for stores. *)

  type key
  (** Type for keys. *)

  type value
  (** Type for values. *)

  val read: t -> key -> value option Lwt.t
  (** Read a value from the store. *)

  val read_exn: t -> key -> value Lwt.t
  (** Same as {!read} but raise [Invalid_argument] if the key does not
      exist. *)

  val mem: t -> key -> bool Lwt.t
  (** Check if a key exists. *)

  val iter: t -> (key -> value Lwt.t -> unit Lwt.t) -> unit Lwt.t
  (** [iter t fn] call the function [fn] on all [t]'s keys and
      values. *)

end

(** Append-only store. *)
module type AO = sig

  (** {1 Append-only stores} *)

  include RO

  val create: config -> t Lwt.t
  (** [create config] is a function returning fresh store
      handles, with the configuration [config], which is
      provided by the backend. The operation might be blocking,
      depending on the backend. *)

  val add: t -> value -> key Lwt.t
  (** Write the contents of a value to the store. It's the
      responsibility of the append-only store to generate a
      consistent key. *)

end

(** Immutable Link store. *)
module type LINK = sig

  (** {1 Immutable Link stores}

      The link store contains {i verified} links between low-level
      keys. This is used to certify that a value can be accessed via
      different keys: because they have been obtained using different
      hash functions (SHA1 and SHA256 for instance) or because the
      value might have different but equivalent concrete
      representation (for instance a set might be represented as
      various equivalent trees). *)

  include RO

  val create: config -> t Lwt.t

  val add: t -> key -> value -> unit Lwt.t
  (** [add t src dst] add a link between the key [src] and the value
      [dst]. *)

end

(** Read-write stores. *)
module type RW = sig

  (** {1 Read-write stores} *)

  include RO

  val update: t -> key -> value -> unit Lwt.t
  (** [update t k v] replaces the contents of [k] by [v] in [t]. If
      [k] is not already defined in [t], create a fresh binding.
      Raise [Invalid_argument] if [k] is the {{!Path.empty}empty
      path}. *)

  val compare_and_set: t -> key -> test:value option -> set:value option -> bool Lwt.t
  (** [compare_and_set t key ~test ~set] sets [key] to [set] only if
      the current value of [key] is [test] and in that case returns
      [true]. If the current value of [key] is different, it returns
      [false]. [None] means that the value does not have to exist or
      is removed.

      {b Note:} The operation is guaranteed to be atomic. *)

  val remove: t -> key -> unit Lwt.t
  (** [remove t k] remove the key [k] in [t]. *)

end

(** Hierarchical read-write stores. *)
module type HRW = sig

  (** {1 Hierarchical read-write stores} *)

  (** Hierarchical read-write stores are read-write stores using
      {{!Path.S.t}paths} as keys. They are a very simplified
      abstraction of filesystems. *)

  include RW

  val list: t -> key -> key list Lwt.t
  (** [list t k] list the sub-paths of the path [k] in [t]. *)

  val remove_rec: t -> key -> unit Lwt.t
  (** Same as {{!RW.remove}RW.remove} but removes all the sub-paths
      recursively. *)

end

(** Reactive read-write store *)
module type RRW = sig

  (** {1 Reactive read-write stores} *)

  (** Reactive read-write stores are read-write stores with reactive
      capabilities. *)

  include RW

  type watch
  (** The type of watch handlers. *)

  val watch_key: t -> key -> ?init:value -> (value diff -> unit Lwt.t) ->
    watch Lwt.t
  (** [watch_key t k ?init f] adds [f] to the list of [t]'s watch
      handlers for the key [k] and returns the watch handler to be used
      with {!unwatch}. [init] is the optional initial value of the
      key. *)

  val watch: t -> ?init:(key * value) list -> (key -> value diff -> unit Lwt.t) ->
    watch Lwt.t
  (** [watch t ?init f] adds [f] to the list of [t]'s watch handlers
      and returns the watch handler to be used with {!unwatch}. [init]
      is the optional initial values. It is more efficient to use
      {!watch_key} to watch only a single given key.*)

  val unwatch: t -> watch -> unit Lwt.t
  (** [unwatch t w] removes [w] from [t]'s watch handlers. *)

end

(** Branch-consistent stores. *)
module type BC = sig

  (** {1 Branch-consistent stores}

      Branch-consistent stores are hierarchical read-write stores with
      extended capabilities. They allow an application (or a
      collection of applications) to work with multiple local states,
      which can be forked and merged programmatically, without having
      to rely on a global state. In a way very similar to version
      control systems, Irmin local states are called {i branches}.

      There are two kinds of BC store in Irmin:
      {{!persistent}persistent} named branches and
      {{!temporary}temporary} detached heads. These exist relative to a
      local, larger (and shared) store, and have some (shared)
      contents. This is exactly the same as usual version control
      systems, that the informed user can see as an implicit purely
      functional data-structure.

      {2:persistent Persistent Branches}

      A persistent branch always has a unique ID, which is typically a
      string (the branch name). Thus, in order to use a persistent
      branch, you need to provide its name: see the {{!BC.of_branch_id}of_branch_id}
      function. *)

  module Repo: sig
    (** A repository contains a set of branches. *)

    type t
    (** The type of repository handles. *)

    val create: config -> t Lwt.t
    (** [create config] connects to a repository in a backend-specific manner. *)

    val config: t -> config
    (** Recover the config passed to [create].
     * todo: would be good to remove this, but Ir_sync_ext needs it for now. *)
  end

  include HRW
  (** A branch-consistent store is a hierarchical read-write store. *)

  val master: 'a Task.f -> Repo.t -> ('a -> t) Lwt.t
  (** [master repo task] is a function returning fresh store
      handles within the repository [repo], with fresh tasks
      computed using [task].
      The result is a persistent branch using the {Ref.S.master} reference.
      This operation is cheap, can be repeated multiple times. *)

  val repo: t -> Repo.t
  (** [repo t] is the repository containing [t]. *)

  val task: t -> task
  (** [task t] is the task associated to the store handle [t]. *)

  type branch_id
  (** Type for persistent branch names. Branches usually share a common
      global namespace and it's the user's responsibility to avoid
      name clashes. *)

  val of_branch_id: 'a Task.f -> branch_id -> Repo.t -> ('a -> t) Lwt.t
  (** [of_branch_id t name] is the persistent branch named [name]. Similar to
      [master], but use [name] instead {!Ref.S.master}. *)

  val name: t -> branch_id option Lwt.t
  (** [name t] is [t]'s branch name. Return [None] if [t] is not persistent. *)

  val name_exn: t -> branch_id Lwt.t
  (** Same as {!name} but raise [Invalid_argument] if [t] is not
      persistent. *)

  val branches: t -> branch_id list Lwt.t
  (** The list of all persistent branch names. Similar to to [git
      branch -a].*)

  val remove_branch: t -> branch_id -> unit Lwt.t
  (** [remove_branch t name] removes the branch [name] from the local store.
      Similar to [git branch -D <name>] *)

  val update_branch: t -> branch_id -> unit Lwt.t
  (** [update_branch t src] updates [t]'s contents with the contents of
      the branch named [src]. Can cause data losses as it discard the
      current contents. Similar to [git reset --hard <src>]. *)

  val merge_branch: t -> ?max_depth:int -> ?n:int -> branch_id -> unit Merge.result Lwt.t
  (** [merge_branch t other] merges the contents of the branch named [other]
      into [t]. Similar to [git merge <other>]. *)

  val merge_branch_exn: t -> ?max_depth:int -> ?n:int -> branch_id -> unit Lwt.t
  (** Same as {!merge_branch} but raise {!Merge.Conflict} in case of
      conflict. *)

  (** {2:temporary Temporary Stores}

      Temporary stores do not have stable names: instead they can be
      addressed using the hash of the current commit. These hashes are
      called {{!BC.head}heads} in Irmin. Temporary stores are
      similar to Git's detached heads. In a temporary store, all the
      operations are performed relative to the current head and update
      operations can modify the current head: the current stores's
      head will automatically become the new head obtained while
      performing the update.

      Temporary stores are created using the {!BC.of_head}
      function. *)

  type head
  (** Type for commit identifiers. Similar to Git's commit SHA1s. *)

  val empty: 'a Task.f -> Repo.t -> ('a -> t) Lwt.t
  (** [empty repo task] is a temporary, empty store. Becomes a
      normal temporary store after the first update. *)

  val of_head: 'a Task.f -> head -> Repo.t -> ('a -> t) Lwt.t
  (** Create a temporary store, using the given [head]. The store
      will not persist as it has no persistent branch name. *)

  val head: t -> head option Lwt.t
  (** [head t] is the current head of the store [t]. This works for
      both persistent and temporary stores. In the case of a
      persistent branch, this involves getting the the head associated
      with the branch's ID, so this may block. In the case of a
      temporary store, it simply returns the current head. Returns
      [None] if the store has no contents. Similar to [git
      rev-parse HEAD]. *)

  val head_exn: t -> head Lwt.t
  (** Same as {!head} but raise [Invalid_argument] if the store does
      not have any contents. *)

  val head_ref: t -> [`Branch of branch_id | `Head of head | `Empty]
  (** [head_ref t] is the branch ID that this store tracks (for persistent
      stores), the current [head] commit (for temporary stores), or [`Empty]
      for empty temporary stores. *)

  val heads: t -> head list Lwt.t
  (** [heads t] is the list of all the heads in local store. Similar
      to [git rev-list --all]. *)

  val update_head: t -> head -> unit Lwt.t
  (** [update_head t h] updates [t]'s contents with the contents of
      the head [h]. Can cause data loss as it discards the current
      contents. Similar to [git reset --hard <hash>]. *)

  val fast_forward_head: t -> ?max_depth:int -> ?n:int -> head -> bool Lwt.t
  (** [fast_forward_head t h] is similar to {!update_head} but the
      [t]'s head is updated to [h] only if [h] is stricly in the
      future of [t]'s current head. Return [false] if it is not the
      case. If present, [max_depth] or [n] are used to limit the
      search space of the lowest common ancestors (see {!lcas}). *)

  val compare_and_set_head: t -> test:head option -> set:head option -> bool Lwt.t
  (** Same as {!update_head} but check that the value is [test] before
      updating to [set]. Use {!update} or {!merge} instead if
      possible. *)

  val merge_head: t -> ?max_depth:int -> ?n:int -> head ->
    unit Merge.result Lwt.t
  (** [merge_head t ?max_head ?n head] merges the contents of the
      commit associated to [head] into [t]. [max_depth] is
      the maximal depth used for getting the lowest common
      ancestor. [n] is the maximum number of lowest common
      ancestors. If present, [max_depth] or [n] are used to limit the
      search space of the lowest common ancestors (see {!lcas}). *)

  val merge_head_exn: t -> ?max_depth:int -> ?n:int -> head -> unit Lwt.t
  (** Same as {{!BC.merge_head}merge_head} but raise {!Merge.Conflict}
      in case of a conflict. *)

  val watch_head: t -> ?init:head -> (head diff -> unit Lwt.t) ->
    (unit -> unit Lwt.t) Lwt.t
  (** [watch_branch t f] calls [f] every time the contents of [t]'s reference
      is updated. Do nothing if [t] is not persistent. Return a clean-up
      function to remove the watch handler.

      {b Note:} even [f] might skip some head updates, it will never
      be called concurrently: all consecutive calls to [f] are done in
      sequence, so we ensure that the previous one ended before
      calling the next one. *)

  val watch_branches: t -> ?init:(branch_id * head) list ->
    (branch_id -> head diff -> unit Lwt.t) -> (unit -> unit Lwt.t) Lwt.t
  (** [watch_branches t f] calls [f] every time a branch is added, removed or
      updated in the local store. Return a function to remove the
      handler. *)

  val watch_key: t -> key -> ?init:(head * value) ->
    ((head * value) diff -> unit Lwt.t) -> (unit -> unit Lwt.t) Lwt.t
  (** [watch_key t key f] calls [f] every time the [key]'s value is
      added, removed or updated. *)

  (** {2 Clones and Merges} *)

  val clone: 'a Task.f -> t -> branch_id ->
    [`Ok of ('a -> t) | `Duplicated_branch | `Empty_head] Lwt.t
  (** Clone the store [t], using the given branch name. Return
      [Duplicated_branch] if a branch with the same name already exists
      and [Empty_head] if [t] has no head. *)

  val clone_force: 'a Task.f -> t -> branch_id -> ('a -> t) Lwt.t
  (** Same as {{!BC.clone}clone} but delete and update the existing
      branch if a branch with the same name already exists. *)

  val merge: 'a -> ?max_depth:int -> ?n:int -> ('a -> t) -> into:('a -> t) ->
    unit Merge.result Lwt.t
  (** [merge x t i] merges [t x]'s current branch into [i x]'s current
      branch. After that operation, the two stores are still
      independent. Similar to [git merge <branch>]. *)

  val merge_exn: 'a -> ?max_depth:int -> ?n:int -> ('a -> t) -> into:('a -> t) ->
    unit Lwt.t
  (** FIXME Same as {{!BC.merge}merge} but raise {!Merge.Conflict} in case
      of a conflict. *)

  val lcas: 'a -> ?max_depth:int -> ?n:int -> ('a -> t) -> ('a -> t) ->
    [`Ok of head list | `Max_depth_reached | `Too_many_lcas ] Lwt.t
  (** [lca ?max_depth ?n msg t1 t2] returns the collection of least
      common ancestors between the heads of [t1] and [t2] branches.

      {ul
      {- [max_depth] is the maximum depth of the exploration (default
      is [max_int]). Return [`Max_depth_reached] if this depth is
      exceeded.}
      {- [n] is the maximum expected number of lcas. Stop the
      exploration as soon as [n] lcas are found. Return
      [`Too_many_lcas] if more [lcas] are found. }
      }
  *)

  val lcas_branch: t -> ?max_depth:int -> ?n:int -> branch_id ->
    [`Ok of head list | `Max_depth_reached | `Too_many_lcas] Lwt.t
  (** Same as {!lcas} but takes a branch ID as argument. *)

  val lcas_head: t -> ?max_depth:int -> ?n:int -> head ->
    [`Ok of head list | `Max_depth_reached | `Too_many_lcas] Lwt.t
  (** Same as {!lcas} but takes an head as argument. *)

  (** {2 History} *)

  module History: Graph.Sig.P with type V.t = head
  (** An history is a DAG of heads. *)

  val history: ?depth:int -> ?min:head list -> ?max:head list -> t -> History.t Lwt.t
  (** [history ?depth ?min ?max t] is a view of the history of the
      store [t], of depth at most [depth], starting from the [max]
      (or from the [t]'s head if the list of heads is empty) and
      stopping at [min] if specified. *)

  val task_of_head: t -> head -> task Lwt.t
  (** [task_of_head t h] is the task which created [h]. Useful to
      retrieve the commit date and the committer name. *)

  (** {2 Slices} *)

  type slice
  (** Type for store slices. *)

  val export: ?full:bool -> ?depth:int -> ?min:head list -> ?max:head list ->
    t -> slice Lwt.t
  (** [export t ~depth ~min ~max] exports the store slice between
      [min] and [max], using at most [depth] history depth (starting
      from the max).

      If [max] is not specified, use the current [heads]. If [min] is
      not specified, use an unbound past (but can still be limited by
      [depth]).

      [depth] is used to limit the depth of the commit history. [None]
      here means no limitation.

      If [full] is set (default is true), the full graph, including the
      commits, nodes and contents, is exported, otherwise it is the
      commit history graph only. *)

  val import: t -> slice -> [`Ok | `Error] Lwt.t
  (** [import t s] imports the contents of the slice [s] in [t]. Does
      not modify branches. *)

end

(** {1 User-Defined Contents} *)

(** Store paths.

    An Irmin {{!Irmin.S}store} binds {{!Path.S.t}paths} to
    user-defined {{!Contents.S}contents}. Paths are composed by basic
    elements, that we call {{!Path.S.step}steps}. The following [Path]
    module provides functions to manipulate steps and paths. *)
module Path: sig

  (** {1 Path} *)

  (** Signature for path steps. *)
  module type STEP = Hum.S

  (** Signature for path implementations.*)
  module type S = sig

    (** {1 Path} *)

    type t
    (** The type for path values. *)

    include Hum.S with type t := t

    type step
    (** Type type for path's steps. *)

    val empty: t
    (** The empty path. *)

    val create: step list -> t
    (** Create a path from a list of steps. *)

    val is_empty: t -> bool
    (** Check if the path is empty. *)

    val cons: step -> t -> t
    (** Prepend a step to the path. *)

    val rcons: t -> step -> t
    (** Append a step to the path. *)

    val decons: t -> (step * t) option
    (** Deconstruct the first element of the path. Return [None] if
        the path is empty. *)

    val rdecons: t -> (t * step) option
    (** Deconstruct the last element of the path. Return [None] if the
        path is empty. *)

    val map: t -> (step -> 'a) -> 'a list
    module Step: STEP with type t = step

  end

  module String_list: S with type step = string and type t = string list
  (** An implementation of paths as string lists. *)

end

(** Hashing functions.

    [Hash] provides user-defined hash function to digest serialized
    contents. Some {{!backend}backends} might be parameterized by such
    hash functions, others might work with a fixed one (for instance,
    the Git format use only SHA1).

    An {{!Hash.SHA1}SHA1} implementation is available to pass to the
    backends. *)
module Hash: sig

  (** {1 Contents Hashing} *)

  exception Invalid of string
  (** Exception raised when parsing a human-readable representation of
      a hash. *)

  module type S = sig

    (** Signature for unique identifiers. *)

    include Hum.S

    val digest: Cstruct.t -> t
    (** Compute a deterministic store key from a {!Cstruct.t} value. *)

    val has_kind: [> `SHA1] -> bool
    (** The kind of generated hash. *)

    val to_raw: t -> Cstruct.t
    (** The raw hash value. *)

    val of_raw: Cstruct.t -> t
    (** Abstract a hash value. *)

    val digest_size: int
    (** [digest_size] is the size of hash results, in bytes. *)

  end
  (** Signature for hash values. *)

  module SHA1: S
  (** SHA1 digests *)

end

(** [Contents] specifies how user-defined contents need to be {e
    serializable} and {e mergeable}.

    The user need to provide:

    {ul
    {- a pair of [to_json] and [of_json] functions, to be used by the
    REST interface.}
    {- a triple of [size_of], [write] and [read] functions, to
    serialize data on disk or to send it over the network.}
    {- a 3-way [merge] function, to handle conflicts between multiple
    versions of the same contents.}
    }

    Default contents for {{!Contents.String}string},
    {{!Contents.Json}JSON} and {{!Contents.Cstruct}C-buffers like}
    values are provided. *)
module Contents: sig

  module type S = sig

    (** {1 Signature for store contents} *)

    include Tc.S0
    (** Base functions on contents. *)

    module Path: Path.S
    (** The type for store paths. *)

    val merge: Path.t -> t option Merge.t
    (** Merge function. Evaluates to [`Conflict] if the values cannot
        be merged properly. The arguments of the merge function can
        take [None] to mean that the key does not exists for either
        the least-common ancestor or one of the two merging
        points. The merge function returns [None] when the key's value
        should be deleted. *)

  end

  module String: S with type t = string and module Path = Path.String_list
  (** String values where only the last modified value is kept on
      merge. If the value has been modified concurrently, the [merge]
      function raises [Conflict]. *)

  module Json: S with type t = Ezjsonm.t and module Path = Path.String_list
  (** JSON values where only the last modified value is kept on
      merge. If the value has been modified concurrently, the [merge]
      function raises [Conflict]. *)

  module Cstruct: S with type t = Cstruct.t and module Path = Path.String_list
  (** Cstruct values where only the last modified value is kept on
      merge. If the value has been modified concurrently, the [merge]
      function raises [Conflict]. *)

  (** Contents store. *)
  module type STORE = sig

    include AO

    module Path: Path.S
    (** The type for store paths. *)

    val merge: Path.t -> t -> key option Merge.t
    (** [merge t] lifts the merge functions defined on contents values
        to contents key. The merge function will: {e (i)} read the
        values associated with the given keys, {e (ii)} use the merge
        function defined on values and {e (iii)} write the resulting
        values into the store to get the resulting key. See
        {!Contents.S.merge}.

        If any of these operations fail, return [`Conflict]. *)

    module Key: Hash.S with type t = key
    (** [Key] provides base functions for user-defined contents keys. *)

    module Val: S with type t = value and module Path = Path
    (** [Val] provides base functions for user-defined contents values. *)

  end

  (** [Make] builds a contents store. *)
  module Make (S: sig
                 include AO
                 module Key: Hash.S with type t = key
                 module Val: S with type t = value
               end):
    STORE with type t = S.t
           and type key = S.key
           and type value = S.value
           and module Path = S.Val.Path

end

(** User-defined references. A reference store associates a name (branch ID)
    with its head commit in an Irmin store. *)
module Ref: sig

  (** {1 Refs} *)

  (** An implementation specifies base functions on abstract IDs
      and defines a default value for denoting the
      {{!Ref.S.master}master} branch name. *)
  module type S = sig

    (** {1 Signature for Ref implementations} *)

    (** Signature for reference IDs (i.e. branch names). *)

    include Hum.S

    val master: t
    (** The name of the master branch. *)

    val is_valid: t -> bool
    (** Check if the branch ID is valid. *)

  end

  module String: S with type t = string
  (** [String] is an implementation of {{!Ref.S}S} where branch IDs are
      strings. The [master] branch ID is ["master"]. Valid strings contain
      only alpha-numeric characters, [-], [_], [.], and [/]. *)

  (** [STORE] specifies the signature of reference stores.

      A {i reference store} is a mutable and reactive key / value store,
      where keys are names created by users (and/or global names
      created by convention) and values are keys from the block store.

      A typical Irmin application should have a very low number of
      keys in the reference store. *)
  module type STORE = sig

    (** {1 Ref Store} *)

    include RRW

    val create: config -> t Lwt.t

    module Key: S with type t = key
    (** Base functions on keys. *)

    module Val: Hash.S with type t = value
    (** Base functions on values. *)

  end

end

(** {1 High-level Stores}

    An Irmin store is a branch-consistent store where keys are lists
    of steps.

    An example is a Git repository where keys are filenames, {e i.e.}
    list of ['/']-separated strings. More complex examples are
    structured values, where steps might contain first-class field
    accessors and array offsets.

    Irmin provides the following features:

    {ul
    {- Support for fast {{!BC}clones}, branches and merges, in a
    fashion very similar to Git.}
    {- Efficient {{!View}staging areas} for fast, transient,
    in-memory operations.}
    {- Fast {{!Sync}synchronization} primitives between remote
    stores, using native backend protocols (as the Git protocol) when
    available.}
    }
*)

(** [Private] defines functions only useful for creating new
    backends. If you are just using the library (and not developing a
    new backend), you should not use this module. *)
module Private: sig

(** Backend configuration.

    A backend configuration is a set of {{!keys}keys} mapping to
    typed values. Backends define their own keys. *)
  module Conf: sig

    (** {1 Configuration converters}

        A configuration converter transforms a string value to an OCaml
        value and vice-versa. There are a few
        {{!builtin_converters}built-in converters}. *)

    type 'a parser = string -> [ `Error of string | `Ok of 'a ]
    (** The type for configuration converter parsers. *)

    type 'a printer = Format.formatter -> 'a -> unit
    (** The type for configuration converter printers. *)

    type 'a converter = 'a parser * 'a printer
    (** The type for configuration converters. *)

    val parser: 'a converter -> 'a parser
    (** [parser c] is [c]'s parser. *)

    val printer: 'a converter -> 'a printer
    (** [converter c] is [c]'s printer. *)

    (** {1:keys Keys} *)

    type 'a key
    (** The type for configuration keys whose lookup value is ['a]. *)

    val key: ?docs:string -> ?docv:string -> ?doc:string ->
      string -> 'a converter -> 'a -> 'a key
    (** [key ~docs ~docv ~doc name conv default] is a configuration key named
        [name] that maps to value [default] by default. [conv] is
        used to convert key values provided by end users.

        [docs] is the title of a documentation section under which the
        key is documented. [doc] is a short documentation string for the
        key, this should be a single sentence or paragraph starting with
        a capital letter and ending with a dot.  [docv] is a
        meta-variable for representing the values of the key
        (e.g. ["BOOL"] for a boolean).

        @raise Invalid_argument if the key name is not made of a
        sequence of ASCII lowercase letter, digit, dash or underscore.
        FIXME not implemented.

        {b Warning.} No two keys should share the same [name] as this
        may lead to difficulties in the UI. *)

    val name: 'a key -> string
    (** The key name. *)

    val conv: 'a key -> 'a converter
    (** [tc k] is [k]'s converter. *)

    val default: 'a key -> 'a
    (** [default k] is [k]'s default value. *)

    val doc: 'a key -> string option
    (** [doc k] is [k]'s documentation string (if any). *)

    val docv: 'a key -> string option
    (** [docv k] is [k]'s value documentation meta-variable (if any). *)

    val docs: 'a key -> string option
    (** [docs k] is [k]'s documentation section (if any). *)

    val root: string option key
    (** Default [--root=ROOT] argument. *)

    (** {1:conf Configurations} *)

    type t = config
    (** The type for configurations. *)

    val empty: t
    (** [empty] is the empty configuration. *)

    val singleton: 'a key -> 'a -> t
    (** [singleton k v] is the configuration where [k] maps to [v]. *)

    val is_empty: t -> bool
    (** [is_empty c] is [true] iff [c] is empty. *)

    val mem: t -> 'a key -> bool
    (** [mem c k] is [true] iff [k] has a mapping in [c]. *)

    val add: t -> 'a key -> 'a -> t
    (** [add c k v] is [c] with [k] mapping to [v]. *)

    val rem: t -> 'a key -> t
    (** [rem c k] is [c] with [k] unbound. *)

    val union: t -> t -> t
    (** [union r s] is the union of the configurations [r] and [s]. *)

    val find: t -> 'a key -> 'a option
    (** [find c k] is [k]'s mapping in [c], if any. *)

    val get: t -> 'a key -> 'a
    (** [get c k] is [k]'s mapping in [c].

        {b Raises.} [Not_found] if [k] is not bound in [d]. *)

    (** {1:builtin_converters Built-in value converters}  *)

    val bool: bool converter
    (** [bool] converts values with [bool_of_string].  *)

    val int: int converter
    (** [int] converts values with [int_of_string]. *)

    val string: string converter
    (** [string] converts values with the identity function. *)

    val uri: Uri.t converter
    (** [uri] converts values with {!Uri.of_string}. *)

    val some: 'a converter -> 'a option converter
    (** [string] converts values with the identity function. *)

  end

  (** [Watch] provides helpers to register event notifications on
      read-write stores. *)
  module Watch: sig

    (** {1 Watch Helpers} *)

    (** The signature for watch helpers. *)
    module type S = sig

      (** {1 Watch Helpers} *)

      type key
      (** The type for store keys. *)

      type value
      (** The type for store values. *)

      type watch
      (** The type for watch handlers. *)

      type t
      (** The type for watch state. *)

      val stats: t -> int * int
      (** [stats t] is a tuple [(k,a)] represeting watch stats. [k] is
          the number of single key watchers for the store [t] and [a] the
          number of global watchers for [t]. *)

      val notify: t -> key -> value option -> unit Lwt.t
      (** Notify all listeners in the given watch state that a key has
          changed, with the new value associated to this key. [None]
          means the key has been removed. *)

      val create: unit -> t
      (** Create a watch state. *)

      val clear: t -> unit
      (** Clear all register listeners in the given watch state. *)

      val watch_key: t -> key -> ?init:value -> (value diff -> unit Lwt.t) ->
        watch Lwt.t
      (** Watch a given key for changes. More efficient than {!watch}. *)

      val watch: t -> ?init:(key * value) list ->
        (key -> value diff -> unit Lwt.t) -> watch Lwt.t
      (** Add a watch handler. To watch a specific key, use
          {!watch_key} which is more efficient. *)

      val unwatch: t -> watch -> unit Lwt.t
      (** Remove a watch handler. *)

      val listen_dir: t -> string
        -> key:(string -> key option)
        -> value:(key -> value option Lwt.t)
        -> (unit -> unit) Lwt.t
      (** Register a thread looking for changes in the given directory
          and return a function to stop watching and free up
          resources. *)

    end

    val workers: unit -> int
    (** [workers ()] is the number of background worker threads
        managing event notification currently active. *)

    val set_listen_dir_hook:
      (int -> string -> (string -> unit Lwt.t) -> (unit -> unit) Lwt.t) -> unit
    (** Register a function which looks for file changes in a
        directory and return a function to stop watching. Could use
        [inotify] when available or {!Irmin_unix.set_listen_dir_hook}
        to use active file polling. *)

    (** [Make] builds an implementation of watch helpers. *)
    module Make(K: Tc.S0) (V: Tc.S0): S with type key = K.t and type value = V.t

  end

  module Lock: sig
    (** {1 Process locking helpers} *)

    module type S = sig

      type t
      (** The type for lock manager. *)

      type key
      (** The type for key to be locked. *)

      val create: unit -> t
      (** Create a lock manager. *)

      val with_lock: t -> key -> (unit -> 'a Lwt.t) -> 'a Lwt.t
      (** [with_lock t k f] executes [f ()] while holding the exclusive
          lock associated to the key [k]. *)

    end

    module Make (K: Tc.S0): S with type key = K.t
    (** Create a lock manager implementation. *)

  end

  (** [Node] provides functions to describe the graph-like structured
      values.

      The node blocks form a labeled directed acyclic graph, labeled
      by {{!Path.S.step}steps}: a list of steps defines a
      unique path from one node to an other.

      Each node can point to user-defined {{!Contents.S}contents}
      values. *)
  module Node: sig

    module type S = sig

      (** {1 Node values} *)

      include Tc.S0

      type contents
      (** The type for contents keys. *)

      type node
      (** The type for node keys. *)

      type step
      (** The type for steps between nodes. *)

      val create: (step * [`Contents of contents | `Node of node]) list -> t
      (** [create l] is a new node. *)

      val alist: t -> (step * [`Contents of contents | `Node of node]) list
      (** [alist t] is the contents of [t]. *)

      val empty: t
      (** The empty node. *)

      val is_empty: t -> bool
      (** Is the node empty? *)

      val contents: t -> step -> contents option
      (** Get the node contents.

          A node can point to user-defined
          {{!Node.S.contents}contents}. The edge between the node and
          the contents is labeled by a {{!Node.S.step}step}. *)

      val iter_contents: t -> (step -> contents -> unit) -> unit
      (** [iter_contents t f] calls [f] on [t]'s contents. For better
          performance, use {{!Node.S.contents}contents} instead when
          you know the step in advance. *)

      val with_contents: t -> step -> contents option -> t
      (** [with_contents t s c] replaces [t]'s contents for the step
          [s] by [c]. *)

      val succ: t -> step -> node option
      (** [succ t s] is [s]'s successor in [t]. *)

      val iter_succ: t -> (step -> node -> unit) -> unit
      (** [iter_succ t f] calls f on [t]'s successors. *)

      val with_succ: t -> step -> node option -> t
      (** [replace_succ t s n] replaces [t]'s successor for the step
          [s] by [n]. *)

    end

    (** [Node] provides a simple node implementation, parameterized by
        the contents [C], node [N] and paths [P]. *)
    module Make (C: Tc.S0) (N: Tc.S0) (P: Path.S):
      S with type contents = C.t
         and type node = N.t
         and type step = P.step

    (** [STORE] specifies the signature for node stores. *)
    module type STORE = sig

      include AO

      module Path: Path.S
      (** [Step] provides base functions on node steps. *)

      module Key: Hash.S with type t = key
      (** [Key] provides base functions for node keys. *)

      (** [Val] provides base functions for node values. *)
      module Val: S with type t = value
                     and type node = key
                     and type step = Path.step
    end

    (** [Graph] specifies the signature for node graphs. A node graph
        is a deterministic DAG, labeled by steps. *)
    module type GRAPH = sig

      (** {1 Node Graphs} *)

      type t
      (** The type for store handles. *)

      type contents
      (** The type of user-defined contents. *)

      type node
      (** The type for node values. *)

      type step
      (** The type of steps. A step is used to pass from one node to
          another. *)

      type path
      (** The type of store paths. A path is composed of
          {{!step}steps}. *)

      val empty: t -> node Lwt.t
      (** The empty node. *)

      val create: t -> (step * [`Contents of contents | `Node of node]) list
        -> node Lwt.t
      (** Create a new node. *)

      val contents: t -> node -> step -> contents option Lwt.t
      (** [contents t n s] is [n]'s contents in [t], associated to the
          step [s]. *)

      val succ: t -> node -> step -> node option Lwt.t
      (** [succ t n s] is [n]'s successors in [t], associated to the
          step [s]. *)

      val steps: t -> node -> step list Lwt.t
      (** [steps t n] is the list of steps leaving the node [t]. *)

      val iter_contents: t -> node -> (step -> contents -> unit) -> unit Lwt.t
      (** [iter_contents f fn] calls [fn] on [t]'s contents. *)

      val iter_succ: t -> node -> (step -> node -> unit) -> unit Lwt.t
      (** [iter_succ t fn] calls [fn] on [t]'s successors. *)

      (** {1 Contents} *)

      val mem_contents: t -> node -> path -> bool Lwt.t
      (** [mem_contents t n path] checks if there is a path labeled by
          [path] from [n] to a valid contents in [t].  *)

      val read_contents: t -> node -> path -> contents option Lwt.t
      (** [read_contents t n path] is the contents at the end of the
          path starting from [n] and labeled by [path] in [t]. Return
          [None] if no such contents exists.*)

      val read_contents_exn: t -> node -> path -> contents Lwt.t
      (** Same as {!read_contents} but raises [Invalid_argument] if
          there is no valid contents. *)

      val add_contents: t -> node -> path -> contents -> node Lwt.t
      (** [add_contents t n path c] adds the contents [c] as the end of
          the path starting from [n] and labeled by [path] in [t]. *)

      val remove_contents: t -> node -> path -> node Lwt.t
      (** [remove_contents t n path] removes the contents at the end
          of the path starting from [n] and labeled by [path] in
          [t]. *)

      (** {1 Nodes} *)

      val mem_node: t -> node -> path -> bool Lwt.t
      (** [mem_node t n] checks if there is a path labeled by [path]
          from [n] to a valid node in [t]. *)

      val read_node: t -> node -> path -> node option Lwt.t
      (** [read_node t n path] is the node at the end of the path
          starting from [n] and labeled by [path] in [t]. Return
          [None] if no such node exists. *)

      val read_node_exn: t -> node -> path -> node Lwt.t
      (** Same as {{!Node.GRAPH.read_node}read_node} but raise
          [Invalid_argument] if the path is invalid. *)

      val add_node: t -> node -> path -> node -> node Lwt.t
      (** [add_node t n path c] adds the node [c] as the end of the
          path starting from [n] and labeled by [path] in [t]. *)

      val remove_node: t -> node -> path -> node Lwt.t
      (** [remove_node t n path] removes the node at the end of the
          path of the path starting from [n] and labeled by [path] in
          [t]. *)

      val closure: t -> min:node list -> max:node list -> node list Lwt.t
      (** [closure t ~min ~max] is the transitive closure [c] of [t]'s
          nodes such that:

          {ul
          {- There is a path in [t] from any nodes in [min] to nodes
          in [c]. If [min] is empty, that condition is always true.}
          {- There is a path in [t] from any nodes in [c] to nodes in
          [max]. If [max] is empty, that condition is always false.}
          }

          {B Note:} Both [min] and [max] are subsets of [c].*)

      module Store: Contents.STORE
        with type t = t
         and type key = node
         and type Path.t = path
         and type Path.step = step
      (** Graph nodes forms a {{!Contents.STORE}contents store}. *)

    end

    module Graph (C: Contents.STORE)
        (S: STORE with type Val.contents = C.key and module Path = C.Path)
      : GRAPH with type t = C.t * S.t
               and type contents = C.key
               and type node = S.key
               and type path = S.Path.t
               and type step = S.Path.step

  end

  (** Commit values represent the store history.

      Every commit contains a list of predecessor commits, and the
      collection of commits form an acyclic directed graph.

      Every commit also can contain an optional key, pointing to a
      {{!Private.Commit.STORE}node} value. See the
      {{!Private.Node.STORE}Node} signature for more details on node
      values. *)
  module Commit: sig

    module type S = sig

      (** {1 Commit values} *)

      include Tc.S0
      (** Base functions on commit values. *)

      type commit
      (** Type for commit keys. *)

      type node
      (** Type for node keys. *)

      val create: task -> ?node:node -> parents:commit list -> t
      (** Create a commit. *)

      val node: t -> node option
      (** The underlying node. *)

      val parents: t -> commit list
      (** The commit parents. *)

      val task: t -> task
      (** The commit provenance. *)

    end

    (** [Make] provides a simple implementation of commit values,
        parameterized by the commit [C] and node [N]. *)
    module Make (C: Tc.S0) (N: Tc.S0):
      S with type commit := C.t and type node = N.t

    (** [STORE] specifies the signature for commit stores. *)
    module type STORE = sig

      (** {1 Commit Store} *)

      include AO

      module Key: Hash.S with type t = key
      (** [Key] provides base functions for commit keys. *)

      (** [Val] provides functions for commit values. *)
      module Val: S with type t = value and type commit := key

    end

    (** [History] specifies the signature for commit history. The
        history is represented as a partial-order of commits and basic
        functions to search through that history are provided.

        Every commit can point to an entry point in a node graph, where
        user-defined contents are stored. *)
    module type HISTORY = sig

      (** {1 Commit History} *)

      type t
      (** The type for store handles. *)

      type node
      (** The type for node values. *)

      type commit
      (** The type for commit values. *)

      val create: t -> ?node:node -> parents:commit list -> task:task -> commit Lwt.t
      (** Create a new commit. *)

      val node: t -> commit -> node option Lwt.t
      (** Get the commit node.

          A commit might contain a graph
          {{!Private.Node.GRAPH.node}node}. *)

      val parents: t -> commit -> commit list Lwt.t
      (** Get the commit parents.

          Commits form a append-only, fully functional, partial-order
          data-structure: every commit carries the list of its
          immediate predecessors. *)

      val merge: t -> task:task -> commit Merge.t
      (** [merge t] is the 3-way merge function for commit.  *)

      val lcas: t -> ?max_depth:int -> ?n:int -> commit -> commit ->
        [`Ok of commit list | `Max_depth_reached | `Too_many_lcas ] Lwt.t
      (** Find the lowest common ancestors
          {{:http://en.wikipedia.org/wiki/Lowest_common_ancestor}lca}
          between two commits. *)

      val lca: t -> task:task -> ?max_depth:int -> ?n:int -> commit list ->
        commit option Merge.result Lwt.t
      (** Compute the lowest common ancestors ancestor of a list of
          commits by recursively calling {!lcas} and merging the
          results.

          If one of the merges results in a conflict, or if a call to
          {!lcas} returns either [`Max_depth_reached] or
          [`Too_many_lcas] then the function returns [None]. *)

      val three_way_merge: t -> task:task -> ?max_depth:int -> ?n:int -> commit -> commit ->
        commit Merge.result Lwt.t
      (** Compute the {!lcas} of the two commit and 3-way merge the
          result. *)

      val closure: t -> min:commit list -> max:commit list -> commit list Lwt.t
      (** Same as {{!Private.Node.GRAPH.closure}GRAPH.closure} but for
          the history graph. *)

      (** A history forms a {{!STORE}store} of commits. *)
      module Store: sig
        include STORE with type t = t and type key = commit
        module Path: Ir_path.S
        val merge: Path.t -> t -> task:task -> key option Merge.t
      end

    end

    (** Build a commit history. *)
    module History (N: Contents.STORE) (S: STORE with type Val.node = N.key):
      HISTORY with type t = N.t * S.t
               and type node = N.key
               and type commit = S.key

  end

  (** The signature for slices. *)
  module Slice: sig

    module type S = sig

      (** {1 Slices} *)

      include Tc.S0
      (** Slices are serializable. *)

      type contents
      (** The type for exported contents. *)

      type node
      (** The type for exported nodes. *)

      type commit
      (** The type for exported commits. *)

      val create: unit -> t Lwt.t
      (** Create a new empty slice. *)

      val add_contents: t -> contents -> unit Lwt.t
      (** [add_contents t c] adds the contents [c] to the slice [t]. *)

      val add_node: t -> node -> unit Lwt.t
      (** [add_node t n] adds the node [n] to the slice [t]. *)

      val add_commit: t -> commit -> unit Lwt.t
      (** [add_commit t c] adds the commit [c] to the slice [t]. *)

      val iter_contents: t -> (contents -> unit Lwt.t) -> unit Lwt.t
      (** [iter_contents t f] calls [f] on [t]'s contents. *)

      val iter_nodes: t -> (node -> unit Lwt.t) -> unit Lwt.t
      (** [iter_nodes t f] calls [f] on [t]'s nodes. *)

      val iter_commits: t -> (commit -> unit Lwt.t) -> unit Lwt.t
      (** [iter_commits t f] calls [f] on [t]'s commits. *)

    end

    (** Build simple slices. *)
    module Make (C: Contents.STORE) (N: Node.STORE) (H: Commit.STORE):
      S with type contents = C.key * C.value
         and type node = N.key * N.value
         and type commit = H.key * H.value

  end

  module Sync: sig

    module type S = sig

      (** {1 Remote synchronization} *)

      type t
      (** The type for store handles. *)

      type head
      (** The type for store heads. *)

      type branch_id
      (** The type for branch IDs. *)

      val create: config -> t Lwt.t
      (** Create a remote store handle. *)

      val fetch: t -> ?depth:int -> uri:string -> branch_id ->
        [`Head of head | `No_head | `Error] Lwt.t
      (** [fetch t uri] fetches the contents of the remote store
          located at [uri] into the local store [t]. Return the head
          of the remote branch with the same name, which is now in the
          local store. [No_head] means no such branch exists. *)

      val push: t -> ?depth:int -> uri:string -> branch_id -> [`Ok | `Error] Lwt.t
      (** [push t uri] pushes the contents of the local store [t] into
          the remote store located at [uri]. *)

    end

    (** [None] is an implementation of {{!Private.Sync.S}S} which does
        nothing. *)
    module None (H: Tc.S0) (R: Tc.S0): S with type head = H.t and type branch_id = R.t

  end

  (** The complete collection of private implementations. *)
  module type S = sig

    (** {1 Private Implementations} *)

    (** Private contents. *)
    module Contents: Contents.STORE

    (** Private nodes. *)
    module Node: Node.STORE
      with type Val.contents = Contents.key and module Path = Contents.Path

    (** Private commits. *)
    module Commit: Commit.STORE with type Val.node = Node.key

    (** Private references. *)
    module Ref: Ref.STORE with type value = Commit.key

    (** Private slices. *)
    module Slice: Slice.S
      with type contents = Contents.key * Contents.value
       and type node = Node.key * Node.value
       and type commit = Commit.key * Commit.value

    module Sync: Sync.S with type head = Commit.key and type branch_id = Ref.key

  end

end

(** Signature for Irmin stores. *)
module type S = sig

  (** {1 Irmin Store} *)

  include BC

  module Key: Path.S with type t = key
  (** [Key] provides base functions on paths. *)

  module Val: Contents.S with type t = value
  (** [Val] provides base functions on user-defined, mergeable
      contents. *)

  module Ref: Ref.S with type t = branch_id
  (** [Ref] provides base functions on user-defined references. *)

  module Head: Hash.S with type t = head
  (** [Head] provides base functions on head values. *)

  (** Private functions, which might be used by the backends. *)
  module Private: sig
    include Private.S
      with type Contents.value = value
       and module Contents.Path = Key
       and type Commit.key = head
       and type Ref.key = branch_id
       and type Slice.t = slice
    val repo: t -> Repo.t
    val contents_t: t -> Contents.t
    val node_t: t -> Node.t
    val commit_t: t -> Commit.t
    val ref_t: t -> Ref.t
    val read_node: t -> key -> Node.key option Lwt.t
    val mem_node: t -> key -> bool Lwt.t
    val update_node: t -> key -> Node.key -> unit Lwt.t
    val merge_node: t -> key -> (head * Node.key) -> unit Merge.result Lwt.t
    val remove_node: t -> key -> unit Lwt.t
    val iter_node: t -> Node.key ->
      (key -> value Lwt.t -> unit Lwt.t) -> unit Lwt.t
  end
end

(** [S_MAKER] is the signature exposed by any backend providing {!S}
    implementations. [C] is the implementation of user-defined contents, [R] is
    the implementation of store references and [H] is the implementation of
    store heads. It does not use any native synchronization primitives. *)
module type S_MAKER =
  functor (C: Contents.S) ->
  functor (R: Ref.S) ->
  functor (H: Hash.S) ->
    S with type key = C.Path.t
       and type value = C.t
       and type branch_id = R.t
       and type head = H.t

(** {1:basics Basic API} *)

(** The basic API considers default Irmin implementations using:

    {ul
    {- {{!Path.String_list}list of strings} as keys.}
    {- {{!Ref.String}strings} as branch IDs.}
    {- {{!Hash.SHA1}SHA1} as internal digests.}
    }

    Only the {{!Contents.S}contents} is provided by the user.
*)


module type BASIC = S with type branch_id = string and type head = Hash.SHA1.t
(** The signature of basic stores.
 
    Branch names (refs) are strings and the hash is SHA1.
    To generate a basic store from a backend, use e.g.

    [module Store = Irmin_git.FS(Irmin.Contents.String)(Irmin.Ref.String)(Irmin.Hash.SHA1)]
*)

(** {2 Synchronization} *)

type remote
(** The type for remote stores. *)

val remote_uri: string -> remote
(** [remote_uri s] is the remote store located at [uri]. Use the
    optimized native synchronization protocol when available for the
    given backend. *)


(** {1:examples Examples}

    These examples are in the [examples] directory of the
    distribution.

    {3 Synchronization}

    A simple synchronization example, using the
    {{!Irmin_unix.Irmin_git}Git} backend and the {!Sync} helpers. The
    code clones a fresh repository if the repository does not exist
    locally, otherwise it performs a fetch: in this case, only
    the missing contents is downloaded.

{[
open Lwt
open Irmin_unix

let store = Irmin.basic (module Irmin_git.FS) (module Irmin.Contents.String)
let config = Irmin_git.config ~root:"/tmp/test" ()

let upstream =
  if Array.length Sys.argv = 2 then (Irmin.remote_uri Sys.argv.(1))
  else (Printf.eprintf "Usage: sync [uri]\n%!"; exit 1)

let test () =
  Irmin.create store config task
  >>= fun t  -> Irmin.pull_exn (t "Syncing with upstream store") upstream `Update
  >>= fun () -> Irmin.read_exn (t "get the README") ["README.md"]
  >>= fun r  -> Printf.printf "%s\n%!" r; return_unit

let () =
  Lwt_main.run (test ())
]}

    {3 Mergeable logs}

    We will demonstrate the use of custom merge operators by
    defining mergeable debug log files. We first define a log entry
    as a pair of a timestamp and a message, using the combinator
    exposed by {{:https://github.com/mirage/mirage-tc}mirage-tc}:

{[
  module Entry = struct
    include Tc.Pair (Tc.Int)(Tc.String)
    let compare (x, _) (y, _) = Pervasives.compare x y
    let time = ref 0
    let create message = incr time; !time, message
  end
]}

    A log file is a list of entries (one per line), ordered by
    decreasing order of timestamps. The 3-way [merge] operator for log
    files concatenates and sorts the new entries and prepend them
    to the common ancestor's ones.

{[
  module Log: Irmin.Contents.S with type t = Entry.t list = struct
    module Path = Irmin.Path.String_list
    module S = Tc.List(Entry)
    include S

    (* Get the timestamp of the latest entry. *)
    let timestamp = function
      | [] -> 0
      | (timestamp, _ ) :: _ -> timestamp

    (* Compute the entries newer than the given timestamp. *)
    let newer_than timestamp entries =
      let rec aux acc = function
        | [] -> List.rev acc
        | (h, _) :: _ when h <= timestamp -> List.rev acc
        | h::t -> aux (h::acc) t
      in
      aux [] entries

    let merge_log _path ~old t1 t2 =
      let open Irmin.Merge.OP in
      old () >>| fun old ->
      let old = match old with None -> [] | Some o -> o in
      let ts = timestamp old in
      let t1 = newer_than ts t1 in
      let t2 = newer_than ts t2 in
      let t3 = List.sort Entry.compare (List.rev_append t1 t2) in
      ok (List.rev_append t3 old)

    let merge path = Irmin.Merge.option (module S) (merge_log path)

  end
]}

    {b Note:} The serialisation primitives provided by
    {{:https://github.com/mirage/mirage-tc}mirage-tc}: are not very
    efficient in this case as they parse the file every-time. For real
    usage, you would write buffered versions of [Log.read] and
    [Log.write].

    To persist the log file on disk, we need to choose a backend. We
    show here how to use the on-disk [Git] backend on Unix.

{[
  (* Bring [Irmin_unix.task] and [Irmin_unix.Irmin_git] in scope. *)
  open Irmin_unix

  (* Build an Irmin store containing log files. *)
  let store = Irmin.basic (module Irmin_git.FS) (module Log)

  (* Set-up the local configuration of the Git repository. *)
  let config = Irmin_git.config ~root:"/tmp/irmin/test" ~bare:true ()
]}

  We can now define a toy example to use our mergeable log files.

{[
  open Lwt

  (* Name of the log file. *)
  let file = [ "local"; "debug" ]

  (* Read the entire log file. *)
  let read_file t =
    Irmin.read (t "Reading the log file") file >>= function
    | None   -> return_nil
    | Some l -> return l

  (* Persist a new entry in the log. *)
  let log t fmt =
    Printf.ksprintf (fun message ->
        read_file t >>= fun logs ->
        let logs = Entry.create message :: logs in
        Irmin.update (t "Adding a new entry") file logs
      ) fmt

  let () =
    Lwt_unix.run begin
      Irmin.create store config task
      >>= fun t  -> log t "Adding a new log entry"
      >>= fun () -> Irmin.clone_force task (t "Cloning the store") "x"
      >>= fun x  -> log x "Adding new stuff to x"
      >>= fun () -> log x "Adding more stuff to x"
      >>= fun () -> log x "More. Stuff. To x."
      >>= fun () -> log t "I can add stuff on t also"
      >>= fun () -> log t "Yes. On t!"
      >>= fun () -> Irmin.merge_exn "Merging x into t" x ~into:t
      >>= fun () -> return_unit
    end
]}

*)

(** {1 Helpers} *)

val remote_store: (module S with type t = 'a) -> 'a -> remote
(** [remote_store t] is the remote corresponding to the local store
    [t]. Synchronization is done by importing and exporting store
    {{!BC.slice}slices}, so this is usually much slower than native
    synchronization using {!remote_uri} but it works for all
    backends. *)

(** [SYNC] provides functions to synchronization an Irmin store with
    local and remote Irmin stores. *)
module type SYNC = sig

  (** {1 Native Synchronization} *)

  type db
  (** Type type for store handles. *)

  type head
  (** The type for store heads. *)

  val fetch: db -> ?depth:int -> remote -> [`Head of head | `No_head | `Error] Lwt.t
  (** [fetch t ?depth r] populate the local store [t] with objects for
      the remote store [r], using [t]'s current branch. The [depth]
      parameter limits the history depth. Return [None] if either the
      local or remote store do not have a valid head. *)

  val fetch_exn: db -> ?depth:int -> remote -> head Lwt.t
  (** Same as {!fetch} but raise [Invalid_argument] if either the
      local or remote store do not have a valid head. *)

  val pull: db -> ?depth:int -> remote -> [`Merge | `Update] ->
    [`Ok | `No_head | `Error] Merge.result Lwt.t
  (** [pull t ?depth r s] is similar to {{!Sync.fetch}fetch} but it
      also updates [t]'s current branch. [s] is the update strategy:

      {ul
      {- [`Merge] uses {S.merge_head}. This strategy can return a conflict.}
      {- [`Update] uses {S.update_head.}}
      } *)

  val pull_exn: db -> ?depth:int -> remote -> [`Merge | `Update] -> unit Lwt.t
  (** Same as {!pull} but raise {!Merge.Conflict} in case of
      conflict. *)

  val push: db -> ?depth:int -> remote -> [`Ok | `Error] Lwt.t
  (** [push t ?depth r] populates the remote store [r] with objects
      from the current store [t], using [t]'s current branch. If [b]
      is [t]'s current branch, [push] also updates the head of [b] in
      [r] to be the same as in [t].

      {b Note:} {e Git} semantics is to update [b] only if the new
      head if more recent. This is not the case in {e Irmin}. *)

  val push_exn: db -> ?depth:int -> remote -> unit Lwt.t
  (** Same as {!push} but raise [Invalid_argument] if an error
      happens. *)

end

(** The default [Sync] implementation. *)
module Sync (S: S): SYNC with type db = S.t and type head = S.head

(** [View] provides an in-memory partial mirror of the store, with
    lazy reads and delayed writes.

    Views are like staging area in Git: they are temporary
    non-persistent areas (they disappear if the host crash), held in
    memory for efficiency, where reads are done lazily and writes are
    done only when needed on commit: if you modify a key twice,
    only the last change will be written to the store when you
    commit. Views also hold a list of operations, which are checked
    for conflicts on commits and are used to replay/rebase the view if
    needed. The most important feature of views is that they keep
    track of reads: {e i.e.} you can have a conflict if a view reads a
    key which has been modified concurrently by someone else.  *)
module type VIEW = sig

  (** {1 Views} *)

  type db
  (** The type for store handles. *)

  include HRW
  (** A view is a read-write temporary store, mirroring the main
      store. *)

  val empty: unit -> t Lwt.t
  (** Create an empty view. Empty views do not have associated backend
      configuration values, as they can perform in-memory operation,
      independently of any given backend. *)

  val rebase: t -> into:t -> unit Merge.result Lwt.t
  (** [rebase x t i] rebases the actions done on the view [t x] into
      the view [i x]. If a read operation doesn't return the same
      result, return [Conflict]. Only the view [i] is updated. *)

  val rebase_exn: t -> into:t -> unit Lwt.t
  (** Same as {!rebase} but raise {!Merge.Conflict} in case of
      conflict. *)

  val of_path: db -> key -> t Lwt.t
  (** [of_path t p] reads the view from a path [p] in the branch
      [t]. This is a cheap operation, all the real reads operation
      will be done on-demand when the view is used. If [p] does not
      exist in [t], the the result is an {!empty} view. *)

  val update_path: db -> key -> t -> unit Lwt.t
  (** [update_path t p v] {e replaces} the sub-tree under [p] in the
      branch [t] by the contents of the view [v]. See {!merge_path}
      for more details. *)

  val rebase_path: db -> key -> t -> unit Merge.result Lwt.t
  (** [rebase_path t p v] {e rebases} the view [v] on top of the
      contents of the sub-tree under [p] in the branch [t]. Rebasing
      means re-applying every {{!Action.t}action} stored in [v],
      including the {e reads}. Return {!Merge.Conflict} if one of the
      actions cannot apply cleanly. See {!merge_path} for more
      details.  *)

  val rebase_path_exn: db -> key -> t -> unit Lwt.t
  (** Same as {!rebase_path} but raise {!Merge.Conflict} in case of
      conflict. *)

  val merge_path: db -> ?max_depth:int -> ?n:int -> key -> t ->
    unit Merge.result Lwt.t
  (** [merge_path t path v] {e merges} the view [v] with the contents
      of the sub-tree under [p] in the branch [t]. Merging means
      applying the {{!Merge.Map}merge function for map} between the
      view's contents and [t]'s sub-tree.

      {ul
      {- {!VIEW.update_path} discards any pre-existing subtree.}
      {- {!VIEW.rebase_path} is operation based. It keeps track of read
      operations which can lead to {{!Merge.Conflict}conflicts}. It
      replays the full operation history of the view on top of any
      pre-existing subtree.}
      {- {!VIEW.merge_path} is state based. It is an efficient 3-way merge
      operator between prefix trees, based on {!Merge.Map.merge}.}
      } *)

  val merge_path_exn: db -> ?max_depth:int -> ?n:int -> key -> t -> unit Lwt.t
  (** Same as {!merge_path} but raises {!Merge.Conflict} in case of
      conflicts. *)

  (** [Action] provides information about operations performed on a
      view.

      Each view stores the list of {{!VIEW.Action.t}actions} that
      have already been performed on it. These actions are useful
      when the view needs to be rebased: write operations are
      replayed while read results are checked against the original
      run. *)
  module Action: sig

    (** {1 Actions} *)

    type t =
      [ `Read of (key * value option)
      | `Write of (key * value option)
      | `Rmdir of key
      | `List of (key * key list) ]
    (** Operations on views. The read results are kept to be able
        to replay them on merge and to check for possible conflict:
        this happens if the result read is different from the one
        recorded. *)

    include Tc.S0 with type t := t

    val pretty: t -> string
    (** Pretty-print an action. *)

    val prettys: t list -> string
    (** Pretty-print a sequence of actions. *)

  end
  (** Signature for actions performed on a view. *)

  val actions: t -> Action.t list
  (** Return the list of actions performed on this view since its
      creation. *)

  val diff: t -> t -> (key * value diff) list Lwt.t
  (** Compute the diff between two views. *)

  (** {2 Heads} *)

  type head
  (** The type for commit heads. *)

  val parents: t -> head list
  (** [parents t] are [t]'s parent commits. *)

  val make_head: db -> task -> parents:head list -> contents:t -> head Lwt.t
  (** [make_head t task ~parents ~contents] creates a new commit into
      the store where the branch [t] is stored and return its id (of
      type {!head}). The new commit has [task] as task and the given
      [parents] and [contents]. The actual parents of [contents] are
      not used. *)

  val watch_path: db -> key -> ?init:(head * t) ->
    ((head * t) diff -> unit Lwt.t) -> (unit -> unit Lwt.t) Lwt.t
  (** [watch_head t p f] calls [f] every time a subpath of [p] is
      updated in the branch [t]. The callback parameters contain the
      branch's current head and the corresponding view. *)
end

module View (S: S): VIEW with type db = S.t
                          and type key = S.Key.t
                          and type value = S.Val.t
                          and type head = S.head
(** Create views. *)

val with_hrw_view :
  (module VIEW with type t = 'view and type db = 'store and type key = 'path) ->
  'store ->
  path:'path ->
  [< `Merge | `Rebase | `Update ] ->
  ('view -> unit Lwt.t) -> unit Merge.result Lwt.t
(** [with_rw_view (module View) t ~path strat ops] applies [ops] to an
    in-memory, temporary and mutable view of the store [t].
    All operations in the transaction are relative to [path].
    The [strat] strategy decides which merging strategy to use: see
    {!VIEW.update_path}, {!VIEW.rebase_path} and {!VIEW.merge_path}. *)

(** [Dot] provides functions to export a store to the Graphviz `dot`
    format. *)
module Dot (S: S): sig

  (** {1 Dot Export} *)

  val output_buffer:
    S.t -> ?html:bool -> ?depth:int -> ?full:bool -> date:(int64 -> string) ->
    Buffer.t -> unit Lwt.t
    (** [output_buffer t ?html ?depth ?full buf] outputs the Graphviz
        representation of [t] in the buffer [buf].

        [html] (default is false) enables HTML labels.

        [depth] is used to limit the depth of the commit history. [None]
        here means no limitation.

        If [full] is set (default is not) the full graph, including the
        commits, nodes and contents, is exported, otherwise it is the
        commit history graph only. *)

end

(** {1:backend Backends} *)

(** API to create new Irmin backends. A backend is an implementation
    exposing either a concrete implementation of {!S} or a functor
    providing {!S} once applied.

    There are two ways to create a concrete {!Irmin.S} implementation:

    {ul
    {- {!Make} creates a store where all the objects are stored in the
    same store, using the same internal keys format and a custom binary
    format based on {{:https://github.com/janestreet/bin_prot}bin_prot},
    with no native synchronization primitives: it is usually what is
    needed to quickly create a new backend.}
    {- {!Make_ext} creates a store with a {e deep} embedding of each
    of the internal stores into separate store, with a total control over
    the binary format and using the native synchronization protocols
    when available. This is mainly used by the Git backend, but could
    be used for other similar backends as well in the future.}
    }
*)

(** [AO_MAKER] is the signature exposed by append-only store
    backends. [K] is the implementation of keys and [V] is the
    implementation of values. *)
module type AO_MAKER =
  functor (K: Hash.S) ->
  functor (V: Tc.S0)  ->
    AO with type key = K.t and type value = V.t

(** [RAW] is the signature for raw values. *)
module type RAW = Tc.S0 with type t = Cstruct.t

(** [AO_MAKER_RAW] if the signature exposed by any backend providing
     append-only stores with access to raw values. [K] is the
     implementation of keys and [V] is the implementation of raw
     values. *)
module type AO_MAKER_RAW =
  functor (K: Hash.S) ->
  functor (V: RAW) ->
  AO with type key = K.t and type value = V.t

(** [LINK_MAKER] is the signature exposed by store which enable adding
    relation between keys. This is used to decouple the way keys are
    manipulated by the Irmin runtime and the keys used for
    storage. This is useful when trying to optimize storage for
    random-access file operations or for encryption. *)
module type LINK_MAKER = functor (K: Hash.S) ->
  LINK with type key = K.t and type value = K.t

(** [RW_MAKER] is the signature exposed by read-write store
    backends. [K] is the implementation of keys and [V] is the
    implementation of values.*)
module type RW_MAKER =
  functor (K: Hum.S) ->
  functor (V: Tc.S0) -> sig
    include RRW with type key = K.t and type value = V.t
    val create: config -> t Lwt.t
  end

module Make (AO: AO_MAKER) (RW: RW_MAKER): S_MAKER
(** Simple store creator. Use the same type of all of the internal
    keys and store all the values in the same store. *)

(** Advanced store creator. *)
module Make_ext (P: Private.S): S
  with type key = P.Contents.Path.t
   and type value = P.Contents.value
   and type branch_id = P.Ref.key
   and type head = P.Ref.value
   and type Key.step = P.Contents.Path.step
