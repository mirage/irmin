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

(** The {!Irmin} module provides a common interface and types used by all
    backends.

    The prinicipal concept is the {i store} (see {!S}), which provides access to
    persistently stored values, commits and branches. *)

val version : string
(** The version of the library. *)

(** {1:stores Stores}

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

(** {2 Schema} *)

module Type = Repr
(** Dynamic types for Irmin values, supplied by
    {{:https://github.com/mirage/repr} [Repr]}. These values can be derived from
    type definitions via [\[@@deriving irmin\]] (see the
    {{:https://github.com/mirage/irmin/blob/main/README_PPX.md} documentation
    for [ppx_irmin]})*)

module Hash = Hash
(** Hashing functions.

    [Hash] provides user-defined hash functions to digest serialized contents.
    Some {{!backend} backends} might be parameterized by such hash functions,
    others might work with a fixed one (for instance, the Git format uses only
    {{!Hash.SHA1} SHA1}).

    A {{!Hash.SHA1} SHA1} implementation is available to pass to the backends. *)

module Branch = Branch

module Info = Info
(** Commit info are used to keep track of the origin of write operations in the
    stores. [Info] models the metadata associated with commit objects in Git. *)

module Node = Node
module Commit = Commit

module Metadata = Metadata
(** [Metadata] defines metadata that is attached to contents but stored in
    nodes. For instance, the Git backend uses this to indicate the type of file
    (normal, executable or symlink). *)

module Path = Path
(** Store paths.

    An Irmin {{!Irmin.S} store} binds {{!Path.S.t} paths} to user-defined
    {{!Contents.S} contents}. Paths are composed by basic elements, that we call
    {{!Path.S.step} steps}. The following [Path] module provides functions to
    manipulate steps and paths. *)

module Contents = Contents
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

module Schema = Schema
(** Store schemas *)

(** {2 Common Stores} *)

(** [KV] is similar to {!S} but chooses sensible implementations for path and
    branch. *)
module type KV = sig
  include Store.KV
  (** @inline *)
end

module Json_tree : Store.Json_tree

(** {2 Creating Stores} *)

(** [Maker] is the signature exposed by any backend providing {!S}
    implementations. {!Maker.Make} is parameterised by {!Schema.S}. It does not
    use any native synchronization primitives. *)
module type Maker = sig
  include Store.Maker
  (** @inline *)
end

(** [KV_maker] is like {!Maker} but where everything except the contents is
    replaced by sensible default implementations. {!KV_maker.Make} is
    parameterised by {!Contents.S} *)
module type KV_maker = sig
  include Store.KV_maker
  (** @inline *)
end

(** {2 Generic Key Stores} *)

module Key = Key

(** "Generic key" stores are Irmin stores in which the backend may not be keyed
    directly by the hashes of stored values. See {!Key} for more details. *)
module Generic_key : sig
  include module type of Store.Generic_key
  (** @inline *)

  module type Maker_args = sig
    module Contents_store : Indexable.Maker_concrete_key2
    module Node_store : Indexable.Maker_concrete_key1
    module Commit_store : Indexable.Maker_concrete_key1
    module Branch_store : Atomic_write.Maker
  end

  module Maker (X : Maker_args) :
    Maker
      with type ('h, 'v) contents_key = ('h, 'v) X.Contents_store.key
       and type 'h node_key = 'h X.Node_store.key
       and type 'h commit_key = 'h X.Commit_store.key
end

(** {1 Backends}

    A backend is an implementation exposing either a concrete implementation of
    {!S} or a functor providing {!S} once applied. *)

type config = Conf.t
(** The type for backend-specific configuration values.

    Every backend has different configuration options, which are kept abstract
    to the user. *)

(** {2 Low-level Stores} *)

(** An Irmin backend is built from a number of lower-level stores, each
    implementing fewer operations, such as {{!Content_addressable.Store}
    content-addressable} and {{!Atomic_write.Store} atomic-write} stores. *)

module Read_only = Read_only
(** Read-only backend backends. *)

module Append_only = Append_only
(** Append-only backend backends. *)

module Indexable = Indexable
(** Indexable backend backends. *)

module Content_addressable = Content_addressable
(** Content-addressable backends. *)

module Atomic_write = Atomic_write
(** Atomic-write stores. *)

(** [Maker] uses the same type for all internal keys and store all the values in
    the same store. *)
module Maker (CA : Content_addressable.Maker) (AW : Atomic_write.Maker) :
  Maker with type endpoint = unit

(** [KV_maker] is like {!module-Maker} but uses sensible default implementations
    for everything except the contents type. *)
module KV_maker (CA : Content_addressable.Maker) (AW : Atomic_write.Maker) :
  KV_maker
    with type endpoint = unit
     and type metadata = unit
     and type info = Info.default

(** {2 Backend} *)

(** [Backend] defines functions only useful for creating new backends. If you
    are just using the library (and not developing a new backend), you should
    not use this module. *)
module Backend : sig
  module Conf : module type of Conf
  (** Backend configuration.

      A backend configuration is a set of {{!keys} keys} mapping to typed
      values. Backends define their own keys. *)

  module Watch = Watch
  module Lock = Lock
  module Lru = Lru
  module Slice = Slice
  module Remote = Remote

  module type S = Backend.S
  (** The modules that define a complete Irmin backend. Apply an implementation
      to {!Of_backend} to create an Irmin store. *)
end

(** [Of_backend] gives full control over store creation through definining a
    {!Backend.S}. *)
module Of_backend (B : Backend.S) :
  Generic_key.S
    with module Schema = B.Schema
     and type repo = B.Repo.t
     and type slice = B.Slice.t
     and type contents_key = B.Contents.Key.t
     and type node_key = B.Node.Key.t
     and type commit_key = B.Commit.Key.t
     and module Backend = B

(** {2 Storage} *)

module Storage = Storage
(** [Storage] provides {!Storage.Make} for defining a custom storage layer that
    can be used to create Irmin stores. Unlike {!Backend.S}, an implementation
    of {!Storage.Make} is only concerned with storing and retrieving keys and
    values. It can be used to create stores for {!Backend.S} through something
    like {!Storage.Content_addressable} or, primarily, with {!Of_storage} to
    automatically construct an Irmin store. *)

(** [Of_storage] uses a custom storage layer and chosen hash and contents type
    to create a key-value store. *)
module Of_storage (M : Storage.Make) (H : Hash.S) (V : Contents.S) :
  KV with type hash = H.t and module Schema.Contents = V

(** {2 Helpers} *)

module Perms = Perms

module Export_for_backends = Export_for_backends
(** Helper module containing useful top-level types for defining Irmin backends.
    This module is relatively unstable. *)

(** {1 Advanced} *)

(** {2 Custom Merge Operators} *)

module Merge = Merge
(** [Merge] provides functions to build custom 3-way merge operators for various
    user-defined contents. *)

module Diff = Diff
(** Differences between values. *)

type 'a diff = 'a Diff.t
(** The type for representing differences betwen values. *)

(** {3 Example} *)

(** The complete code for the following can be found in
    [examples/custom_merge.ml].

    We will demonstrate the use of custom merge operators by defining mergeable
    debug log files. We first define a log entry as a pair of a timestamp and a
    message, using the combinator exposed by {!Irmin.Type}:

    {[
      open Lwt.Infix
      open Astring

      let time = ref 0L
      let failure fmt = Fmt.kstr failwith fmt

      (* A log entry *)
      module Entry : sig
        include Irmin.Type.S

        val v : string -> t
        val timestamp : t -> int64
      end = struct
        type t = { timestamp : int64; message : string } [@@deriving irmin]

        let compare x y = Int64.compare x.timestamp y.timestamp

        let v message =
          time := Int64.add 1L !time;
          { timestamp = !time; message }

        let timestamp t = t.timestamp

        let pp ppf { timestamp; message } =
          Fmt.pf ppf "%04Ld: %s" timestamp message

        let of_string str =
          match String.cut ~sep:": " str with
          | None -> Error (`Msg ("invalid entry: " ^ str))
          | Some (x, message) -> (
              try Ok { timestamp = Int64.of_string x; message }
              with Failure e -> Error (`Msg e))

        let t = Irmin.Type.like ~pp ~of_string ~compare t
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
        type t = Entry.t list [@@deriving irmin]

        let empty = []
        let pp_entry = Irmin.Type.pp Entry.t
        let lines ppf l = List.iter (Fmt.pf ppf "%a\n" pp_entry) (List.rev l)

        let of_string str =
          let lines = String.cuts ~empty:false ~sep:"\n" str in
          try
            List.fold_left
              (fun acc l ->
                match Irmin.Type.of_string Entry.t l with
                | Ok x -> x :: acc
                | Error (`Msg e) -> failwith e)
              [] lines
            |> fun l -> Ok l
          with Failure e -> Error (`Msg e)

        let t = Irmin.Type.like ~pp:lines ~of_string t
        let timestamp = function [] -> 0L | e :: _ -> Entry.timestamp e

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
          let t3 =
            List.sort (Irmin.Type.compare Entry.t) (List.rev_append t1 t2)
          in
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
      module Store = Irmin_unix.Git.FS.KV (Log)

      (* Set-up the local configuration of the Git repository. *)
      let config = Irmin_git.config ~bare:true Config.root

      (* Convenient alias for the info function for commit messages *)
      let info = Irmin_unix.info
    ]}

    We can now define a toy example to use our mergeable log files.

    {[
      let log_file = [ "local"; "debug" ]

      let all_logs t =
        Store.find t log_file >|= function None -> Log.empty | Some l -> l

      (** Persist a new entry in the log. Pretty inefficient as it reads/writes
          the whole file every time. *)
      let log t fmt =
        Printf.ksprintf
          (fun message ->
            all_logs t >>= fun logs ->
            let logs = Log.add logs (Entry.v message) in
            Store.set_exn t ~info:(info "Adding a new entry") log_file logs)
          fmt

      let print_logs name t =
        all_logs t >|= fun logs ->
        Fmt.pr "-----------\n%s:\n-----------\n%a%!" name (Irmin.Type.pp Log.t)
          logs

      let main () =
        Config.init ();
        Store.Repo.v config >>= fun repo ->
        Store.main repo >>= fun t ->
        (* populate the log with some random messages *)
        Lwt_list.iter_s
          (fun msg -> log t "This is my %s " msg)
          [ "first"; "second"; "third" ]
        >>= fun () ->
        Printf.printf "%s\n\n" what;
        print_logs "lca" t >>= fun () ->
        Store.clone ~src:t ~dst:"test" >>= fun x ->
        log x "Adding new stuff to x" >>= fun () ->
        log x "Adding more stuff to x" >>= fun () ->
        log x "More. Stuff. To x." >>= fun () ->
        print_logs "branch 1" x >>= fun () ->
        log t "I can add stuff on t also" >>= fun () ->
        log t "Yes. On t!" >>= fun () ->
        print_logs "branch 2" t >>= fun () ->
        Store.merge_into ~info:(info "Merging x into t") x ~into:t >>= function
        | Ok () -> print_logs "merge" t
        | Error _ -> failwith "conflict!"

      let () = Lwt_main.run (main ())
    ]} *)

(** {2 Synchronization} *)

type remote = Remote.t = ..
(** The type for remote stores. *)

val remote_store : (module Generic_key.S with type t = 'a) -> 'a -> remote
(** [remote_store t] is the remote corresponding to the local store [t].
    Synchronization is done by importing and exporting store {{!BC.slice}
    slices}, so this is usually much slower than native synchronization using
    {!Store.remote} but it works for all backends. *)

module Sync = Sync
(** Remote synchronisation. *)

(** {3 Example} *)

(** A simple synchronization example, using the {{!Irmin_unix.Git} Git} backend
    and the {!Sync} helpers. The code clones a fresh repository if the
    repository does not exist locally, otherwise it performs a fetch: in this
    case, only the missing contents are downloaded.

    The complete code for the following can be found in [examples/sync.ml].

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
        S.Repo.v config >>= S.main >>= fun t ->
        Sync.pull_exn t upstream `Set >>= fun () ->
        S.get t [ "README.md" ] >|= fun r -> Printf.printf "%s\n%!" r

      let () = Lwt_main.run (test ())
    ]} *)

(** {1 Helpers} *)

(** [Dot] provides functions to export a store to the Graphviz `dot` format. *)
module Dot (S : Generic_key.S) : Dot.S with type db = S.t

module Metrics = Metrics
(** Type agnostics mechanisms to manipulate metrics. *)
