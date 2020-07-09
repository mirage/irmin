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

(** Content-addressable backend store. *)
module type CONTENT_ADDRESSABLE_STORE = sig
  include S.CONTENT_ADDRESSABLE_STORE
  (** @inline *)
end

(** Append-only backend store. *)
module type APPEND_ONLY_STORE = sig
  include S.APPEND_ONLY_STORE
  (** @inline *)
end

(** Atomic-write stores. *)
module type ATOMIC_WRITE_STORE = sig
  include S.ATOMIC_WRITE_STORE
  (** @inline *)
end

(** {1 User-Defined Contents} *)

(** Store paths.

    An Irmin {{!Irmin.S} store} binds {{!Path.S.t} paths} to user-defined
    {{!Contents.S} contents}. Paths are composed by basic elements, that we call
    {{!Path.S.step} steps}. The following [Path] module provides functions to
    manipulate steps and paths. *)
module Path : sig
  (** {1 Path} *)

  (** Signature for path implementations.*)
  module type S = sig
    include S.PATH
    (** @inline *)
  end

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

  (** Signature for hash values. *)
  module type S = sig
    include S.HASH
    (** @inline *)
  end

  (** Signature for typed hashes, where [hash] directly takes a value as
      argument and incremental hashing is not possible. *)
  module type TYPED = sig
    include S.TYPED_HASH
    (** @inline *)
  end

  include module type of Hash
  (** @inline *)
end

(** [Metadata] defines metadata that is attached to contents but stored in
    nodes. The Git backend uses this to indicate the type of file (normal,
    executable or symlink). *)
module Metadata : sig
  module type S = sig
    include S.METADATA
    (** @inline *)
  end

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
  module type S = sig
    include S.CONTENTS
    (** @inline *)
  end

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

  (** Contents store. *)
  module type STORE = sig
    include S.CONTENTS_STORE
    (** @inline *)
  end

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

  (** The signature for branches. Irmin branches are similar to Git branches:
      they are used to associated user-defined names to head commits. Branches
      have a default value: the {{!Branch.S.master} master} branch. *)
  module type S = sig
    include S.BRANCH
    (** @inline *)
  end

  module String : S with type t = string
  (** [String] is an implementation of {{!Branch.S} S} where branches are
      strings. The [master] branch is ["master"]. Valid branch names contain
      only alpha-numeric characters, [-], [_], [.], and [/]. *)

  (** [STORE] specifies the signature for branch stores.

      A {i branch store} is a mutable and reactive key / value store, where keys
      are branch names created by users and values are keys are head commmits. *)
  module type STORE = sig
    include S.BRANCH_STORE
    (** @inline *)
  end
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
    module type S = sig
      include S.NODE
      (** @inline *)
    end

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
    module V1 (S : S with type step = string) : sig
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

  module Sync = Sync

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
  include Store.S
  (** @inline *)
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
     and type Private.Sync.endpoint = unit

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

(** [SYNC] provides functions to synchronize an Irmin store with local and
    remote Irmin stores. *)
module type SYNC = sig
  include S.SYNC_STORE
  (** @inline *)
end

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
          exit 1)

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
              with Failure e -> Error (`Msg e))

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
     and type Private.Sync.endpoint = unit

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
