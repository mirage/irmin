(*
 * Copyright (c) 2013-2014 Thomas Gazagnaire <thomas@gazagnaire.org>
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

(** A library for persistent stores following the same design
    principle as Git.

    Irmin is a distributed and history-preserving library for
    persistent stores with built-in snapshot, branching and reverting
    mechanisms. It is designed to use a large variety of
    backends. Irmin is written in pure OCaml and does not depend on
    external C stubs; it aims is to run everywhere, from Linux to Xen
    unikernels -- and can be be compiled to JavaScipt to run in a
    browser.

    FIXME
*)

(** {1 User-Defined Contents} *)

(** Defining the {e contents} of the store and how {e merge} conflicts
    between different version of the same contents should be
    resolved. *)

(** [Merge] provides functions to build custom 3-way merge operators
    for various user-defined contents. *)
module Merge: sig

  (** {1 Merge Results} *)

  type 'a result = [ `Ok of 'a | `Conflict of string ]
  (** Type for merge results. *)

  module Result: Tc.I1 with type 'a t = 'a result
  (** Base functions over results. *)

  val bind: 'a result Lwt.t -> ('a -> 'b result Lwt.t) -> 'b result Lwt.t
  (** Monadic bind over Result. *)

  exception Conflict of string
  (** Exception which might be raised when merging.  *)

  val exn: 'a result -> 'a Lwt.t
  (** Convert [`Conflict] results to [Conflict] exceptions. *)

  (** {1 Merge Combinators} *)

  type 'a t = old:'a -> 'a -> 'a -> 'a result Lwt.t
  (** Signature of a merge function.

      {v
              /----> t1 ----\
      ----> old              |--> result
              \----> t2 ----/
      v}
  *)

  module type S = Tc.I0

  type 'a elt = (module S with type t = 'a)
  (** The type for mergeable contents of type ['a]. *)

  val default: 'a elt -> 'a t
  (** Create a default merge function. This is a simple merge
      functions which support changes in one branch at the time:

      {ul
        {- if [t1=t2] then the result of the merge is [`OK t1];}
        {- if [t1=old] then the result of the merge is [`OK t2];}
        {- if [t2=old] then return [`OK t1];}
        {- otherwise the result is [`Conflict].}
      }
  *)

  val string: string t
  (** The default string merge function. Do not anything clever, just
      compare the strings using the [default] merge function. *)

  val counter: int t
  (** The merge function for mergeable counters. *)

  val seq: 'a t list -> 'a t
  (** Try the merge functions in sequence until one does not raise a conflict. *)

  val some: 'a elt -> 'a t -> 'a option t
  (** Lift a merge function to optional values of the same type. If all
      the provided values are inhabited, then call the provided merge
      function, otherwise use the same behavior as [create]. *)

  val alist: 'a elt -> 'b elt -> 'b t -> ('a * 'b) list t
  (** List to association lists. *)

  (** Lift to maps. *)
  module Map (M: Map.S) (X: S with type t = M.key): sig

    (** {1 Merging Maps} *)

    val merge: 'a elt -> 'a t -> 'a M.t t
    (** Lift to [X.t] maps. *)

  end

  val pair: 'a elt -> 'b elt -> 'a t -> 'b t -> ('a * 'b) t
  (** Lift to pairs. *)

  val biject: 'a elt -> 'b elt -> 'a t -> ('a -> 'b) -> ('b -> 'a) -> 'b t
  (** Use the merge function defined in another domain. If the
      functions given in argument are partial (i.e. returning
      [Not_found] on some entries), the exception is caught and
      [Conflict] is returned instead. *)

  val biject':
    'a elt -> 'b elt -> 'a t -> ('a -> 'b Lwt.t) -> ('b -> 'a Lwt.t) -> 'b t
  (** Same as [map] but with potentially blocking converting
      functions. *)

  val apply: ('a -> 'b t) -> 'a -> 'b t
  (** The [apply] combinator is useful to untie recursive loops. *)

  (** Useful merge operators.

      Use [open Irmin.Merge.OP] at the top of your file to use
      them. *)
  module OP: sig

    (** {1 Useful operators} *)

    val ok: 'a -> 'a result Lwt.t
    (** Return [`Ok x]. *)

    val conflict: ('a, unit, string, 'b result Lwt.t) format4 -> 'a
    (** Return [`Conflict str]. *)

    val (>>|): 'a result Lwt.t -> ('a -> 'b result Lwt.t) -> 'b result Lwt.t
    (** Same as [bind]. *)

  end

end

(** [Contents] specifies how user-defined contents need to be {e
    serializable} and {e mergeable}.

    The user need to provide:

    {ul
    {- a [to_sexp] function for debugging purposes (that might expose
      the internal state of abstract values)}
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

    (** Signature for store contents. *)

    include Tc.I0
    (** Base functions over contents. *)

    val merge: t Merge.t
    (** Merge function. Evaluates to [`Conflict] if the values cannot be
        merged properly. *)

  end

  module String: S with type t = string
  (** String values where only the last modified value is kept on
      merge. If the value has been modified concurrently, the [merge]
      function raises [Conflict]. *)

  module Json: S with type t = Ezjsonm.t
  (** JSON values where only the last modified value is kept on
      merge. If the value has been modified concurrently, the [merge]
      function raises [Conflict]. *)

  module Cstruct: S with type t = Cstruct.t
  (** Cstruct values where only the last modified value is kept on
      merge. If the value has been modified concurrently, then this is a
      conflict. *)

end

(** {1 Stores} *)

(** Irmin provides to the user a high-level store, with few
    interesting features:

    {ul
    {- Support for fast {{!BC}clones}, branches and merges, in a
    fashion very similar to Git.}
    {- Efficient {{!S.View}staging areas} for fast, transient,
    in-memory operations.}
    {- Space efficient {{!S.Snapshot}snapshots} and fast and consistent
    rollback operations.}
    {- Fast {{!S.Sync}synchronization} primitives between remote
    stores, using native backend protocols (as the Git protocol) when
    available.}
    }

    An Irmin store is automatically built from a number of lower-level
    stores, implementing fewer operations, such as {{!AO}append-only}
    and {{!RW}read-write} stores. These low-level stores are provided
    by various backends. *)

(** Tasks are used to keep track of the origin of reads and writes in
    the store. Every high-level operation is expected to have its own
    task, which is passed to every low-level calls. *)
module Task: sig

  (** {1 Task} *)

  type t
  (** The type for tasks. Every task has a date, a text message and an
      name identifying the entity performing that operation. Tasks are
      threaded from high-level calls to all low-level store
      operations. A task might also contains some secrets, which will
      never be serialized.  *)

  include Tc.I0 with type t := t

  val create: date:int64 -> owner:string -> ('a, unit, string, t) format4 -> 'a
  (** Create a new task. *)

  val fprintf: t ->  ('a, unit, string, unit) format4 -> 'a
  (** Add a message to the task messages list. *)

  val date: t -> int64
  (** Get the task date. *)

  val uid: t -> int64
  (** Get the task unique identifier. *)

  val owner: t -> string
  (** Get the task owner. *)

  val messages: t -> string list
    (** Get the messages associated to the task. *)

end

(** Universal values.

    See {{:http://mlton.org/UniversalType}http://mlton.org/UniversalType}

    Universal values are used to carry around configuration values, see
    {{!RO.create}RO.create}. *)
module Univ: sig

  (** {1 Universal value} *)

  type t
  (** Type type for universal values. *)

  val create: unit -> ('a -> t) * (t -> 'a option)
  (** [create ()] returns a function to inject and a function to
      project a value from a given type in to/from a universal
      value. *)

end

(** Read-only stores. *)
module type RO = sig

  (** {1 Read-only stores} *)

  type t
  (** Type for stores. *)

  type key
  (** Type for keys. *)

  type value
  (** Type for values. *)

  val create: (string * Univ.t) list -> Task.t -> t
  (** [create config task] is the store handle with the
      configuration [config] and the task [task]. *)

  val config: t -> (string * Univ.t) list
  (** [config t] is the list of configurations keys for the store
      handle [t]. *)

  val task: t -> Task.t
  (** [task t] is the task associated to the store handle [t]. *)

  val read: t -> key -> value option Lwt.t
  (** Read a value from the store. *)

  val read_exn: t -> key -> value Lwt.t
  (** Same as [read] but raise [Not_found] if the key does not
      exist. *)

  val mem: t -> key -> bool Lwt.t
  (** Check if a key exists. *)

  val list: t -> key list -> key list Lwt.t
  (** [list t keys] is the list of sub-keys that any of the keys in
      [keys] are allowed to access. *)

  val dump: t -> (key * value) list Lwt.t
  (** [dump t] is a dump of the store contents. *)

end

(** Append-only store. *)
module type AO = sig

  (** {1 Append-only stores} *)

  include RO

  val add: t -> value -> key Lwt.t
  (** Write the contents of a value to the store. That's the
      responsibility of the append-only store to generate a
      consistent key. *)

end

(** Read-write stores. *)
module type RW = sig

  (** {1 Read-write stores} *)

  include RO

  val update: t -> key -> value -> unit Lwt.t
  (** Replace the contents of [key] by [value] if [key] is already
      defined and create it otherwise. *)

  val remove: t -> key -> unit Lwt.t
  (** Remove the given key. *)

  val watch: t -> key -> value option Lwt_stream.t
  (** Watch the stream of values associated to a given key. Return
      [None] if the value is removed. *)

end

(** Branch-consistent stores. *)
module type BC = sig

  (** {1 Branch-consistent Store}

      They are two kinds of branch consistent stores: the
      {{!persistent}persistent} and the {{!temporary}temporary} ones.

      {2:persistent Persistent Stores}

      The persistent stores are associated to a branch name, or
      {{!BC.tag}tag}. The tag value is updated every time the
      store is updated, so every handle connected or which will be
      connected to the same tag will see the changes.

      These stores can be created using the
      {{!BC.of_tag}of_tag} functions. *)

  include RW
  (** A branch-consistent store is read-write.

      [create config task] is a persistent store handle on the
      [master] branch. This operation is cheap, can be repeated
      multiple times and is expected to be done for every new user
      task. *)

  type tag
  (** Type for branch names, or tags. Tags usually share a common
      global namespace and that's the user responsibility to avoid
      name-clashes. *)

  val of_tag: (string * Univ.t) list -> Task.t -> tag -> t
  (** [create t tag] is a persistent store handle. Similar to
      [create], but use the [tag] branch instead of the [master]
      one. *)

  val tag: t -> tag option
  (** [tag t] is the tag associated to the store handle [t]. [None]
      means that the branch is not persistent. *)

  val tag_exn: t -> tag
  (** Same as [tag] but raise [Not_found] if the store handle is not
      persistent. *)

  val update_tag: t -> tag -> [`Ok | `Duplicated_tag] Lwt.t
  (** Change the current tag name. Fail if a tag with the same name
      already exists. The head is unchanged. *)

  val update_tag_force: t -> tag -> unit Lwt.t
  (** Same as [update_tag] but delete and update the tag if it already
      exists. *)

  val switch: t -> tag -> unit Lwt.t
  (** Switch the store contents the be same as the contents of the
      given branch name. The two branches are still independent. *)

  val detach: t -> unit Lwt.t
  (** Detach the current branch (i.e. it is not associated to a tag
      anymore). *)

  (** {2:temporary Temporary Stores}

      The temporary stores do not use global branch names. Instead,
      the operations are relative to a given store revision: a
      {{!BC.head}head}. Every operation updates the store as a
      normal persistent store, but the value of head is only kept
      into the local store handle and it is not persisted into the
      store -- this means it cannot be easily shared by concurrent
      processes or loaded back in the future. In the Git
      terminology, these store handle are said to be {i detached
      heads}. *)

  type head
  (** Type for head values. *)

  val of_head: (string * Univ.t) list -> Task.t -> head -> t
  (** Create a temporary store handle, which will not persist as it
      has no associated to any persistent tag name. *)

  val head: t -> head option Lwt.t
  (** Return the head commit. This works for both persistent and
      temporary stores. In the case of a persistent store, this
      involves looking into the value associated to the branch tag,
      so this might blocks. In the case of a temporary store, it is
      a simple (non-blocking) look-up in the store handle local
      state. *)

  val head_exn: t -> head Lwt.t
  (** Same as [read_head] but raise [Not_found] if the commit does
      not exist. *)

  val heads: t -> head list Lwt.t
  (** The list of all the database heads. *)

  val update_head: t -> head -> unit Lwt.t
  (** Set the commit head. *)

  val merge_head: t -> head -> unit Merge.result Lwt.t
  (** Merge a commit with the current branch. *)

  val watch_head: t -> key -> (key * head) Lwt_stream.t
  (** Watch changes for a given collection of keys and the ones they
      have recursive access. Return the stream of heads corresponding
      to the modified keys. *)

  (** {2 Clones and Merges} *)

  val clone: t -> tag -> [`Ok of t | `Duplicated_tag] Lwt.t
  (** Fork the store, using the given branch name. Return [None] if
      the branch already exists. *)

  val clone_force: t -> tag -> t Lwt.t
  (** Same as [clone] but delete and update the existing branch if a
      branch with the same name already exists. *)

  val merge: t -> tag -> unit Merge.result Lwt.t
  (** [merge db t] merges the branch [t] into the current store
      branch. The two branches are still independent. *)

  (** {2 Slices} *)

  type slice
  (** Type for store slices. *)

  module Slice: Tc.I0 with type t = slice
  (** Base functions over slices. *)

  val export: ?full:bool -> ?depth:int -> ?min:head list -> ?max:head list ->
    t -> slice Lwt.t
  (** [export t ~depth ~min ~max] exports the store slice between
      [min] and [max], using at most [depth] history depth (starting
      from the max).

      If [max] is not specified, use the current [heads]. If [min] is
      not specified, use an unbound past (but can be still limited by
      [depth]).

      [depth] is used to limit the depth of the commit history. [None]
      here means no limitation.

      If [full] is set (default is true) the full graph, including the
      commits, nodes and contents, is exported, otherwise it is the
      commit history graph only. *)

  val import: t -> slice -> [`Ok | `Duplicated_tags of tag list] Lwt.t
  (** Import a store slide. Do not modify existing tags. *)

  val import_force: t -> slice -> unit Lwt.t
  (** Same as [import] but delete and update the tags they already
      exist in the store. *)

end

(** Irmin high-level stores.

    An irmin store is a branch-consistent store where keys are lists
    of steps.

    An example is a Git repository where keys are filenames, i.e. list
    of ['\']-separated strings. More complex examples are structured
    values, where steps might contains first-class fields accessors
    and array offsets. *)
module type S = sig

  (** {1 Irmin Store} *)

  type step
  (** The type for step values. *)

  include BC with type key = step list

  module Step: Tc.I0 with type t = step
  (** [Step] provides base functions over steps. *)

  module Key: Tc.I0 with type t = key
  (** [Key] provides base functions over step lists. *)

  module Val: Contents.S with type t = value
  (** [Val] provides base functions over user-defined, mergeable
      contents. *)

  (** [View] provides an in-memory partial mirror of the store, with
      lazy reads and delayed write.

      Views are like staging area in Git: they are temporary
      non-persistent areas (they disappear if the host crash), hold in
      memory for efficiency, where reads are done lazily and writes
      are done only when needed on commit: if if you modify a key
      twice, only the last change will be written to the store when
      you commit. Views also hold a list of operations, which are
      checked for conflicts on commits and are used to replay/rebase
      the view if needed. The most important feature of views is that
      they keep track of reads: i.e. you can have a conflict if a view
      reads a key which has been modified concurrently by someone
      else.  *)
  module View: sig

    (** {1 Views} *)

    type db = t
    (** The type for store handles. *)

    include RW with type key = Key.t and type value = Val.t
    (** A view is a read-write temporary store, mirroring the main
        store. *)

    val create: Task.t -> t
    (** The empty view. *)

    val merge: t -> into:t -> unit Merge.result Lwt.t
    (** Merge the actions done on one view into an other one. If a read
        operation doesn't return the same result, return
        [Conflict]. Only the [into] view is updated. *)

    val of_path: db -> key -> t Lwt.t
    (** Read a view from a path in the store. This is a cheap operation,
        all the real reads operation will be done on-demand when the
        view is used. *)

    val update_path: db -> key -> t -> unit Lwt.t
    (** Commit a view to the store. The view *replaces* the current
        subtree, so if you want to do a merge, you have to do it
        manually (by creating a new branch, or rebasing before
        committing). *)

    val rebase_path: db -> key -> t -> unit Merge.result Lwt.t
    (** Rebase the view to the tip of the store. *)

    val merge_path: db -> key -> t -> unit Merge.result Lwt.t
    (** Same as [update_path] but *merges* with the current subtree. *)

    (** [Action] provides information about operations performed on a
        view.

        Each view stores the list of {{!S.View.Action.t}actions} that
        have already been performed on it. These actions are useful
        when the view needs to be rebased: write operations are
        replayed while read results are checked against the original
        run. *)
    module Action: sig

      (** {1 Actions} *)

      type t =
         [ `Read of (key * value option)
         | `Write of (key * value option)
         | `List of (key list * key list) ]
       (** Operations on view. The read results are kept to be able
           to replay them on merge and to check for possible conflict:
           this happens if the result read is different from the one
           recorded. *)

       include Tc.I0 with type t := t

       val pretty: t -> string
       (** Pretty-print an action. *)

       val prettys: t list -> string
       (** Pretty-print a sequence of actions. *)

    end
    (** Signature for actions performed on a view. *)

    val actions: t -> Action.t list
    (** Return the list of actions performed on this view since its
        creation. *)

  end

  (** [Snapshot] provides read-only, space-efficient, checkpoints of a
      store. It also provides functions to rollback to a previous
      state. *)
  module Snapshot: sig

    (** {1 Snapshots} *)

    type db = t
    (** Type for store handles. *)

    include RO with type key = Key.t and type value = Val.t
    (** A snapshot is a read-only store, mirroring the main store. *)

    val create: db -> t Lwt.t
    (** Snapshot the current state of the store. *)

    val revert: db -> t -> unit Lwt.t
    (** Revert the store to a previous state. *)

    val merge: db -> t -> unit Merge.result Lwt.t
    (** Merge the given snapshot into the current branch of the
        store. *)

    val watch: db -> key -> (key * t) Lwt_stream.t
    (** Subscribe to the stream of modification events attached to a
        given path. Takes and returns a new snapshot every time a
        sub-path is modified. *)

  end

  (** [Dot] provides functions to export a store to the Graphviz `dot`
      format. *)
  module Dot: sig

    (** {1 Dot Export} *)

    type db = t
    (** The type for store handles. *)

    val output_buffer:
      db -> ?html:bool -> ?depth:int -> ?full:bool -> date:(int64 -> string) ->
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

  (** [Sync] provides functions to synchronization an Irmin store with
      local and remote Irmin stores. *)
  module Sync: sig

    (** {1 Synchronization} *)

    type db = t
    (** The type for store handlers. *)

    type remote
    (** The type for remote stores. *)

    val uri: string -> remote
    (** [uri s] is the remote store located at [uri]. Use the
        optimized native synchronization protocol when available for the
        given backend. *)

    val store: db -> remote
    (** [store t] is the remote corresponding to the local store
        [t]. Synchronization is done by importing and exporting store
        {{!BC.slice}slices}, so this is usually much slower than native
        synchronization using [uri] remotes. *)

    val fetch: db -> ?depth:int -> remote -> head option Lwt.t
    (** [create t last] fetch an object in the local database. The local
        database can then be either [merged], or [updated] to the new
        contents. The [depth] parameter limits the history depth.*)

    val fetch_exn: db -> ?depth:int -> remote -> head Lwt.t
    (** Same as [create] but raise [Failure] is the fetch operation
        fails. *)

    val push: db -> ?depth:int -> remote -> head option Lwt.t
    (** [push t f] push the contents of the current branch of the
        database to the remote database -- also update the remote branch
        with the same name as the local one to points to the new
        state. *)

    val push_exn: db -> ?depth:int -> remote -> head Lwt.t
    (** Same as [push] but raise [Failure] is the push operation
        fails. *)

  end

end

(** {1 Backends} *)

(** [Hash] provides function to digest user-defined serialized
    contents. Some backends might be parameterize by such a hash
    functions, other might work with a fixed one (for instance, the
    Git format use only SHA1).

    An {{!Hash.SHA1}SHA1} implementation is available to pass to the
    backends. *)
module Hash: sig

  (** {1 Contents Hashing} *)

  module type S = sig

    (** Signature for unique identifiers. *)

    include Tc.I0

    val digest: Cstruct.t -> t
    (** Compute a deterministic store key from a cstruct value. *)

  end
  (** Signature for hash values. *)

  module SHA1: S
  (** SHA1 digests *)

end



(*
(** {2 Backends} *)

module Backend: sig

  (** {2 Basic Store Makers} *)

  module Maker: sig

    module type RO =
      functor (K: Tc.I0) ->
      functor (V: Tc.I0) ->
        RO with type key = K.t and type value = V.t
    (** Signature for functor creating read-only stores. *)

  end

  (** {2 Binary stores} *)

  module Binary: sig

    module type RO = RO with type key = Cstruct.t and type value = Cstruct.t
    (** Binary read-only stores. Keys, values and origin are cstruct
        buffers. *)

    module RO (S: RO) (K: Tc.I0) (V: Tc.I0): Maker.RO
    (** Create a typed read-only store from a binary one. *)

  end

  (** {2 JSON stores} *)

  module Json: sig

    module type RO = RO with type key = Ezjsonm.t and type value = Ezjsonm.t
    (** Binary read-only stores. Keys, values and origin are cstruct
        buffers. *)

    module RO (S: RO) (K: Tc.I0) (V: Tc.I0): Maker.RO
    (** Create a typed read-only store from a JSON one. *)

  end

end
*)
