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

(** [Irmin] is a library for persistent stores following the same
    design principle as Git.

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

  module Result: Tc.S1 with type 'a t = 'a result
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

  val default: 'a Tc.t -> 'a t
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

  val some: 'a Tc.t -> 'a t -> 'a option t
  (** Lift a merge function to optional values of the same type. If all
      the provided values are inhabited, then call the provided merge
      function, otherwise use the same behavior as [create]. *)

  val alist: 'a Tc.t -> 'b Tc.t -> 'b t -> ('a * 'b) list t
  (** List to association lists. *)

  (** Lift to maps. *)
  module Map (M: Map.S) (X: Tc.S0 with type t = M.key): sig

    (** {1 Merging Maps} *)

    val merge: 'a Tc.t -> 'a t -> 'a M.t t
    (** Lift to [X.t] maps. *)

  end

  val pair: 'a Tc.t -> 'b Tc.t -> 'a t -> 'b t -> ('a * 'b) t
  (** Lift to pairs. *)

  val biject: 'a Tc.t -> 'b Tc.t -> 'a t -> ('a -> 'b) -> ('b -> 'a) -> 'b t
  (** Use the merge function defined in another domain. If the
      functions given in argument are partial (i.e. returning
      [Not_found] on some entries), the exception is caught and
      [Conflict] is returned instead. *)

  val biject':
    'a Tc.t -> 'b Tc.t -> 'a t -> ('a -> 'b Lwt.t) -> ('b -> 'a Lwt.t) -> 'b t
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

    (** {1 Signature for store contents} *)

    include Tc.S0
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

(** Serializable data with reversible human-readable
    representations. *)
module type HUM = sig

  (** {1 Serializable values with inversible human-readable
      representations} *)

  include Tc.S0

  val to_hum: t -> string
  (** Display a value using its human readable representation. *)

  val of_hum: string -> t
  (** Convert an human readable representation of a value into its
      abstract value.

      @raise Invalid_argument if the string does not represent
      anything meaningful. *)

end

(** User-defined tags. Tags are used to specify branch names in an
    Irmin store. *)
module Tag: sig

  (** {1 Tags} *)

  (** A tag implementations specifies base functions over abstract
      tags and define a default value for denoting the
      {{!Tag.S.master}master} branch name. *)
  module type S = sig

    (** {1 Signature for tags implementations} *)

    (** Signature for tags (i.e. branch names). *)

    include HUM

    val master: t
    (** The name of the master branch. *)

  end

  module Path: S with type t = string list
  (** [Path] is an implementation of {{!Tag.S}S} where tags are lists
      of strings.

      The [master] tag is [["master"]] and the human-representation of
      [["x"];["y"]] is ["x/y"]. *)

end

(** A key in an {{!Irmin.S}stores} is a path of basic elements. We
    call these elements {e steps}, and the following [Path] module
    provides functions to manipulate steps and paths. *)
module Path: sig

  (** {1 Path} *)

  (** Signature for path steps. *)
  module type STEP = HUM

  (** Signature for path implementations.*)
  module type S = sig

    (** {1 Path} *)

    type step
    (** Type type for basic steps. *)

    type t = step list
    (** The type for path values. *)

    module Step: STEP with type t = step

    include HUM with type t := t

  end

  module Make (S: STEP): S with type step = S.t
  (** A list of steps, representing keys in an Irmin store. *)

  module String: S with type step = string
  (** An implementation of paths using strings as steps. *)

end

(** [Hash] provides user-defined hash function to digest serialized
    contents. Some {{!backend}backends} might be parameterize by such
    a hash functions, other might work with a fixed one (for instance,
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

    include HUM

    val digest: Cstruct.t -> t
    (** Compute a deterministic store key from a cstruct value. *)

  end
  (** Signature for hash values. *)

  module SHA1: S
  (** SHA1 digests *)

end

(** {1 Stores} *)

type task
(** The type for user-defined tasks. See {{!Task}Task}. *)

type config
(** The type for backend-specific configuration values. Every backend
    has different configuration options, which are kept abstract to
    the user. *)

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

  (** Backend-specific configuration values. *)
module Config: sig

  (** {1 Configuration value} *)

  type t = config
  (** The type for backend-specific configuration values. *)

  type univ
  (** Type type for universal values.

      See {{:http://mlton.org/UniversalType}http://mlton.org/UniversalType}

      Universal values are used to carry around configuration values, see
      {{!RO.create}RO.create}. *)

  val univ: 'a Tc.t -> ('a -> univ) * (univ -> 'a option) * univ Tc.t
  (** Creation of universal values. [univ tc] returns:

      {ul
      {- a function to inject a value from a given type in to a universal value;}
      {- a function to project from a universal value to a value of a given type;}
      {- a type-class to show and serialize universal values.}
      } *)

  val of_dict: (string * univ) list -> t
  (** Convert a dictionary of universal values into an abstract store
      config. *)

  val to_dict: t -> (string * univ) list
  (** Convert a configuration value into a dictionary of universal
      values. *)

  val find: t -> string -> (univ -> 'a option) -> 'a option
  (** Find a the value associated to a key in a config value. *)

  val find_bool: t -> string -> (univ -> bool option) -> default:bool -> bool
  (** Find a boolean value associated to a key in a config value. *)

end

(** Tasks are used to keep track of the origin of reads and writes in
    the store. Every high-level operation is expected to have its own
    task, which is passed to every low-level calls. *)
module Task: sig

  (** {1 Task} *)

  include Tc.S0 with type t = task

  val create: date:int64 -> owner:string -> ('a, unit, string, t) format4 -> 'a
  (** Create a new task. *)

  val date: t -> int64
  (** Get the task date.

      The date is computed by the user user when calling the
      {{!Task.create}create} function. When available,
      [Unix.gettimeofday ()] is a good value for such date. On more
      esoteric platforms, any monotonic counter is a fine value as
      well. On the Git backend, the date will be translated into the
      commit {e Date} field. *)

  val owner: t -> string
  (** Get the task owner.

      The owner identifies the entity (human, unikernel, process,
      thread, etc) performing an operation. For the Git backend, this
      will be directly translated into the {e Author} field. *)

  val uid: t -> int64
  (** Get the task unique identifier.

      The user does not have control over the generation of that
      unique identifier. That identifier is useful for debugging
      purposes, for instance to relate debug lines to the tasks which
      cause them, and might appear in one line of the commit message
      for the Git backend. *)

  val messages: t -> string list
  (** Get the messages associated to the task.

      Text messages can be added to a task either at creation time,
      using {{!Task.create}create}, or can be appended on already
      created tasks using the {{!Task.fprintf}fprintf} function. For
      the Git backend, this will be translated to the commit
      message.  *)


  val fprintf: t ->  ('a, unit, string, unit) format4 -> 'a
  (** Add a message to the task messages list. See
      {{!Task.messages}messages} for more details. *)

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

  val create: config -> task -> t Lwt.t
  (** [create config task] is the store handle with the configuration
      [config] and the task [task]. [config] is provided by the
      backend and [task] is the provided by the user. *)

  val config: t -> config
  (** [config t] is the list of configurations keys for the store
      handle [t]. *)

  val task: t -> task
  (** [task t] is the task associated to the store handle [t]. *)

  val read: t -> key -> value option Lwt.t
  (** Read a value from the store. *)

  val read_exn: t -> key -> value Lwt.t
  (** Same as [read] but raise [Not_found] if the key does not
      exist. *)

  val mem: t -> key -> bool Lwt.t
  (** Check if a key exists. *)

  val list: t -> key -> key list Lwt.t
  (** [list t key] is the list of sub-keys that the key [keys] is
      allowed to access. *)

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

  val of_tag: config -> task -> tag -> t Lwt.t
  (** [create t tag] is a persistent store handle. Similar to
      [create], but use the [tag] branch instead of the [master]
      one. *)

  val tag: t -> tag option
  (** [tag t] is the tag associated to the store handle [t]. [None]
      means that the branch is not persistent. *)

  val tag_exn: t -> tag
  (** Same as [tag] but raise [Not_found] if the store handle is not
      persistent. *)

  val tags: t -> tag list Lwt.t
  (** The list of all the tags of the store. *)

  val update_tag: t -> tag -> [`Ok | `Duplicated_tag] Lwt.t
  (** Change the current tag name. Fail if a tag with the same name
      already exists. The head is unchanged. *)

  val update_tag_force: t -> tag -> unit Lwt.t
  (** Same as [update_tag] but delete and update the tag if it already
      exists. *)

  val switch: t -> tag -> unit Lwt.t
  (** Switch the store contents the be same as the contents of the
      given branch name. The two branches are still independent. *)

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

  val of_head: config -> task -> head -> t Lwt.t
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

  val branch: t -> [`Tag of tag | `Head of head]
  (** [branch t] is the current branch of the store [t]. Can either be
      a persistent store with a [tag] name or a detached [head]. *)

  val heads: t -> head list Lwt.t
  (** The list of all the heads of the store. *)

  val detach: t -> unit Lwt.t
  (** Detach the current branch (i.e. it is not associated to a tag
      anymore). *)

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
  (** Import a store slide. Do not modify existing tags. FIXME: do not modify tags at all. *)

  val import_force: t -> slice -> unit Lwt.t
  (** Same as [import] but delete and update the tags they already
      exist in the store. *)

end

(** [Private] defines functions only useful for creating new
    backends. If you are just using the library (and not developing a
    new backend), you should not use this module. *)
module Private: sig

  (** API to create new Irmin backends.

      There are two ways to create a concrete {!Irmin.S} implementation.

      {!Make} creates a store where all the objects are stored in the
      same store, using the same internal keys format and a custom
      binary format based on
      {{:https://github.com/janestreet/bin_prot}bin_prot}, with no
      native synchronization primitives: it is usually what is needed
      to quickly create a new backend.

      {!Make_ext} creates a store with a {e deep} embedding of each of
      the internal stores into separate store, with a total control over
      the binary format and using the native synchronization protocols
      when available. This is mainly used by the Git backend, but could
      be used for other similar backends as well in the future.

  *)

  module Contents: sig

    module type STORE = sig

      include AO

      module Key: Hash.S with type t = key
      (** [Key] provides base functions for user-defined contents keys. *)

      module Val: Contents.S with type t = value
      (** [Val] provides base function for user-defined contents values. *)

    end

  end

  (** [Node] provides functions to describe the graph-like structured
      values.

      The node blocks form a labeled directed acyclic graph, labeled
      by {{!Private.Node.STORE.Step}steps}: a list of steps defines a
      unique path from one node to an other.

      Every node can contain some optional key, corresponding to
      user-defined {{!Contents.S}contents} values. *)
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

      val contents: t -> step -> contents option
      (** [contents t s] are the (optional) keys of the node contents
          for the step [s]. *)

      val all_contents: t -> (step * contents) list
      (** List all the contents (slow). *)

      val with_contents: t -> step -> contents option -> t
      (** Replace the contents. *)

      val succ: t -> step -> node option
      (** Extract the successors of a node. *)

      val all_succ: t -> (step * node) list
      (** List all the nodes (slow). *)

      val with_succ: t -> step -> node option -> t
      (** Replace the successors. *)

      val steps: t -> step list
      (** The list of available steps to visit a successor. *)

      val edges: t -> [> `Contents of contents | `Node of node] list
      (** Return the list of successor vertices. *)

      val empty: t
      (** The empty node. *)

      val create: contents:(step * contents) list -> succ:(step * node) list -> t
      (** [create contents succ] is the node with contents [contents]
          and successors [succs]. *)

      val is_empty: t -> bool
      (** Is the node empty. *)
    end

    module Make (C: Tc.S0) (N: Tc.S0) (P: Path.S):
      S with type contents = C.t
         and type node = N.t
         and type step = P.step

    module type STORE = sig

      include AO

      module Path: Path.S
      (** [Step] provides base functions over node steps. *)

      module Key: Hash.S with type t = key
      (** [Key] provides base functions for node keys. *)

      (** [Val] provides base functions for node values. *)
      module Val: S with type t = value
                     and type node = key
                     and type step = Path.step
    end

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
      (** Base functions over commit values. *)

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

      val edges: t -> [> `Node of node | `Commit of commit] list
      (** The graph edges. *)

    end

    module Make (C: Tc.S0) (N: Tc.S0):
      S with type commit := C.t
         and type node = N.t

    module type STORE = sig

      include AO

      module Key: Hash.S with type t = key
      (** [Key] provides base functions for commit keys. *)

      (** [Val] provides function for commit values. *)
      module Val: S with type t = value and type commit := key

    end

  end

  (** Tags defines branch names.

      A *tag store* is a key / value store, where keys are names
      created by users (and/or global names created by convention) and
      values are keys from the block store.

      A typical Irmin application should have a very low number of
      keys in the tag store. *)
  module Tag: sig

    module type STORE = sig

      (** {1 The tag store} *)

      include RW

      module Key: Tag.S with type t = key
      (** Base functions over keys. *)

      module Val: Hash.S with type t = value
      (** Base functions over values. *)

    end

  end

  (** The signature for slices. *)
  module Slice: sig

    module type S = sig

      (** {1 Slices} *)

      include Tc.S0
      (** Slices are serializable. *)

      type contents
      (** The type for exported contents. *)

      type nodes
      (** The type for exported nodes. *)

      type commits
      (** The type for exported commits. *)

      type tags
      (** The type for exported tags. *)

      val create:
        ?contents:contents -> ?nodes:nodes -> ?commits:commits -> ?tags:tags ->
        unit -> t
      (** Create a new slice. *)

      val contents: t -> contents
      (** The slice contents. *)

      val nodes: t -> nodes
      (** The slice nodes. *)

      val commits: t -> commits
      (** The slice commits. *)

      val tags: t -> tags
      (** The slice tags. *)

    end

    (** Build simple slices. *)
    module Make
        (C: Contents.STORE) (N: Node.STORE) (H: Commit.STORE) (T: Tag.STORE):
      S with type contents = (C.key * C.value) list
         and type nodes = (N.key * N.value) list
         and type commits = (H.key * H.value) list
         and type tags = (T.key * T.value) list

  end

  module Sync: sig

    module type S = sig

      (** {1 Remote synchronization} *)

      type t
      (** The type for store handles. *)

      type head
      (** The type for store heads. *)

      type tag
      (** The type for store tags. *)

      val create: config -> t Lwt.t
      (** Create a remote store handle. *)

      val fetch: t -> ?depth:int -> uri:string -> tag ->
        [`Local of head] option Lwt.t
      (** [fetch t uri] fetches the contents of the remote store
          located at [uri] into the local store [t]. Return the head
          of the remote branch with the same name, which is now in the
          local store. [None] is no such branch exists. *)

      val push : t -> ?depth:int -> uri:string -> tag -> [`Ok | `Error] Lwt.t
      (** [push t uri] pushes the contents of the local store [t] into
          the remote store located at [uri]. *)

    end

    (** [None] is an implementation of {{!Private.Sync.S}S} which does
        nothing. *)
    module None (H: Tc.S0) (T: Tc.S0): S with type head = H.t and type tag = T.t

  end

  (** The complete collection of private implementations. *)
  module type S = sig

    (** {1 Private Implementations} *)

    (** Private contents. *)
    module Contents: Contents.STORE

    (** Private nodes. *)
    module Node: Node.STORE with type Val.contents = Contents.key

    (** Private commits. *)
    module Commit: Commit.STORE with type Val.node = Node.key

    (** Private tags. *)
    module Tag: Tag.STORE with type value = Commit.key

    (** Private slices. *)
    module Slice: Slice.S
      with type contents = (Contents.key * Contents.value) list
       and type nodes = (Node.key * Node.value) list
       and type commits = (Commit.key * Commit.value) list
       and type tags = (Tag.key * Tag.value) list

    module Sync: Sync.S with type head = Commit.key and type tag = Tag.key

  end

end

(** {1 High-level Stores}

    An Irmin store is a branch-consistent store where keys are lists
    of steps.

    An example is a Git repository where keys are filenames, i.e. list
    of ['\']-separated strings. More complex examples are structured
    values, where steps might contains first-class fields accessors
    and array offsets.

    FIXME: {!View} {!Snapshot} {!Dot} {!Sync}
*)

(** Signature for Irmin stores. *)
module type S = sig

  (** {1 Irmin Store} *)

  type step
  (** The type for step values. *)

  include BC with type key = step list

  module Key: Path.S with type step = step
  (** [Key] provides base functions over step lists. *)

  module Val: Contents.S with type t = value
  (** [Val] provides base functions over user-defined, mergeable
      contents. *)

  module Tag: Tag.S with type t = tag
  (** [Tag] provides base functions over user-defined tags. *)

  module Head: Hash.S with type t = head
  (** [Head] prives base functions over head values. *)

  (** Private functions, which might be used by the backends. *)
  module Private: sig
    include Private.S
      with type Node.Path.step = step
       and type Contents.value = value
       and type Commit.key = head
       and type Tag.key = tag
       and type Slice.t = slice
    val contents_t: t -> Contents.t
    val node_t: t -> Contents.t * Node.t
    val commit_t: t -> Contents.t * Node.t * Commit.t
    val tag_t: t -> Tag.t
    val read_node: t -> Node.Path.t -> Node.value option Lwt.t
    val mem_node: t -> Node.Path.t -> bool Lwt.t
    val update_node: t -> Node.Path.t -> Node.value -> unit Lwt.t
  end

end

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
module View (S: S): sig

  (** {1 Views} *)

  type db = S.t
  (** The type for store handles. *)

  include RW with type key = S.Key.t and type value = S.Val.t
  (** A view is a read-write temporary store, mirroring the main
      store. *)

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
      | `List of (key * key list) ]
    (** Operations on view. The read results are kept to be able
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

end

(** [Snapshot] provides read-only, space-efficient, checkpoints of a
    store. It also provides functions to rollback to a previous
    state. *)
module Snapshot (S: S): sig

  (** {1 Snapshots} *)

  include RO with type key = S.Key.t and type value = S.Val.t
  (** A snapshot is a read-only store, mirroring the main store. *)

  val create: S.t -> t Lwt.t
  (** Snapshot the current state of the store. *)

  val revert: S.t -> t -> unit Lwt.t
  (** Revert the store to a previous state. *)

  val merge: S.t -> t -> unit Merge.result Lwt.t
  (** Merge the given snapshot into the current branch of the
      store. *)

  val watch: S.t -> key -> (key * t) Lwt_stream.t
  (** Subscribe to the stream of modification events attached to a
      given path. Takes and returns a new snapshot every time a
      sub-path is modified. *)

end

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

(** [Sync] provides functions to synchronization an Irmin store with
    local and remote Irmin stores. *)
module Sync (S: S): sig

  (** {1 Native Synchronization} *)

  type remote
  (** The type for remote stores. *)

  val uri: string -> remote
  (** [uri s] is the remote store located at [uri]. Use the
      optimized native synchronization protocol when available for the
      given backend. *)

  val store: S.t -> remote
  (** [store t] is the remote corresponding to the local store
      [t]. Synchronization is done by importing and exporting store
      {{!BC.slice}slices}, so this is usually much slower than native
      synchronization using [uri] remotes. *)

  val fetch: S.t -> ?depth:int -> remote -> [`Local of S.head] option Lwt.t
  (** [create t last] fetch an object in the local store. The local
      store can then be either [merged], or [updated] to the new
      contents. The [depth] parameter limits the history
      depth. Return the new [head] in the local store corresponding
      to the current branch -- [fetch] does not update the local
      branches, use {{!S.Sync.pull}pull} instead. *)

  val pull: S.t -> ?depth:int -> remote -> [`Merge | `Update] ->
    unit Merge.result Lwt.t
  (** Same as {{!S.Sync.fetch}fetch} but also update the current
      branch. Either [merge] or force [update] with the fetched
      head. *)

  val push: S.t -> ?depth:int -> remote -> [`Ok | `Error] Lwt.t
  (** [push t f] push the contents of the current branch of the
      store to the remote store -- also update the remote branch
      with the same name as the local one to points to the new
      state. *)

end

(** {1:backend Backends} *)

(** A backend is an implementation exposing either a concrete
    implementation of {!S} or a functor providing {!S} once
    applied. *)

(** [AO_MAKER] is the signature exposed by any backend providing
    append-only stores. [K] is the implementation of keys and [V] is
    the implementation of values. *)
module type AO_MAKER =
  functor (K: Hash.S) ->
  functor (V: Tc.S0) ->
    AO with type key = K.t and type value = V.t

(** [RW_MAKER] is the signature exposed by any backend providing
    read-write stores. [K] is the implementation of keys and [V] is
    the implementation of values.*)
module type RW_MAKER =
  functor (K: HUM) ->
  functor (V: Tc.S0) ->
    RW with type key = K.t and type value = V.t

(** [S_MAKER] is the signature exposed by any backend providing {!S}
    implementations. [S] is the type of steps (a key is list of
    steps), [C] is the implementation of user-defined contents, [T] is
    the implementation of store tags and [H] is the implementation of
    store heads. It does not use any native synchronisation
    primitves. *)
module type S_MAKER =
  functor (P: Path.S) ->
  functor (C: Contents.S) ->
  functor (T: Tag.S) ->
  functor (H: Hash.S) ->
    S with type step = P.step
       and type value = C.t
       and type tag = T.t
       and type head = H.t

(** Simple store creator. Use the same type of all of the internal
    keys and store all the values in the same store. *)
module Make (AO: AO_MAKER) (RW: RW_MAKER): S_MAKER

(** Advanced store creator. *)
module Make_ext (P: Private.S): S
  with type step = P.Node.Path.step
   and type value = P.Contents.value
   and type tag = P.Tag.key
   and type head = P.Tag.value

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

    type t
    (** The type for watch state. *)

    val notify: t -> key -> value option -> unit
    (** Notify all listeners in the given watch state that a key has
        changed, with the new value associated to this key. If the
        argument is [None], this means the key has been removed. *)

    val create: unit -> t
    (** Create a watch state. *)

    val clear: t -> unit
    (** Clear all register listeners in the given watch state. *)

    val watch: t -> key -> value option -> value option Lwt_stream.t
    (** Create a stream of value notifications. Need to provide the
        initial value, or [None] if the key does not have associated
        contents yet.  *)

    val listen_dir: t -> string
      -> key:(string -> key option)
      -> value:(key -> value option Lwt.t)
      -> unit
      (** Register a fsevents/inotify thread to look for changes in
          the given directory. *)

  end

  val set_listen_dir_hook: (string -> (string -> unit Lwt.t) -> unit) -> unit
  (** Register a function which looks for file changes in a
      directory. Could use [inotify] when available, or use an active
      stats file polling.*)

  val lwt_stream_lift: 'a Lwt_stream.t Lwt.t -> 'a Lwt_stream.t
  (** Lift a stream out of the monad. *)

  (** [Make] builds an implementation of watch helpers. *)
  module Make(K: Tc.S0) (V: Tc.S0): S with type key = K.t and type value = V.t

end
