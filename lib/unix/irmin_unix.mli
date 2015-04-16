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

(** {1 Unix backends}

    This module provides Irmin backends for Unix applications. The
    currently supported backends are:

    {ul
    {- An {{!Irmin_mem}in-memory} store, internally using hash tables. }
    {- A {{!Irmin_fs}file-system} backend, using
    {{:https://github.com/janestreet/bin_prot}bin_prot} to serialize
    internal structures.}
    {- A fully compatible, bidirectional encoding of Irmin into
    {{!Irmin_git}Git}. You can view and edit your store using both the
    library and your usual Git tools. }
    {- The HTTP {{!module:Irmin_http}clients} and
    {{!module:Irmin_http_server}servers} provides a high-level REST API, with
    1 RTT for the {{!Irmin.S.Private}private} and {{!Irmin.BC}public}
    functions.}
    }

*)

val task: string -> Irmin.task
(** [task fmt] creates a fresh task, with the {{!Irmin.Task.date}date}
    set to [Unix.gettimeoday ()] and a task {{!Irmin.Task.owner}owner}
    based on the local Git configuration {b FIXME:} not implemented,
    use [Unix.gethostname()] and [Unix.getpid()] for now on.  *)

(** File system backends, using
    {{:https://github.com/janestreet/bin_prot}bin_prot}. *)
module Irmin_fs: sig

  (** {1 File-system Store} *)

  val config: ?root:string -> unit -> Irmin.config
  (** Create a configuration value. [root] is the location of local
  repository's root.*)

  module AO: Irmin.AO_MAKER
  (** Append-only store maker. *)

  module RW: Irmin.RW_MAKER
  (** Read-write store maker. *)

  module Make: Irmin.S_MAKER
  (** Irmin store maker. *)

  (** {1 Extended configuration} *)

  (** [Config] provides function to control the filename shapes. *)
  module type Config = sig

    (** Same as [Config] but gives more control on the file
        hierarchy. *)

    val dir: string -> string
    (** [dir root] is the sub-directory to look for the keys. *)

    val file_of_key: string -> string
    (** Convert a key to a filename. *)

    val key_of_file: string -> string
    (** Convert a filename to a key. *)

  end

  module AO_ext (C: Config): Irmin.AO_MAKER
  (** Append-only store maker, with control over the filenames shapes. *)

  module RW_ext (C: Config): Irmin.RW_MAKER
  (** Read-write store maker, with control over the filename shapes. *)

  module Make_ext (Obj: Config) (Ref: Config): Irmin.S_MAKER
  (** Irmin store maker, with control over the filename shapes. *)

end

(** Bidirectional Git backends. *)
module Irmin_git: sig

  (** {1 Git Store} *)

  val config:
    ?root:string -> ?head:Git.Reference.t -> ?bare:bool -> unit -> Irmin.config
  (** Create a configuration value.

      {ul
      {- [root] is the local Git repository's root (the parent of the
      {e .git/} directory).}
      {- [head] is the name of the local Git repository's current
      branch. If set, this will cause the file {i [root]/.git/HEAD} to
      be modified to contain {i ref: refs/heads/[branch]}.}
      {- If [bare] is set (default is {e unset}), then the local Git
      repository's contents will be expanded into the filesystem on
      each update. This might cause some performance issues.}
      } *)

  val head: Git.Reference.t option Irmin.Private.Conf.key
  (** The configuration key to set the local Git repository's current
      branch. See {!Irmin_git.config}. *)

  val bare: bool Irmin.Private.Conf.key
  (** The configuration key to set the local Git repository's bare
      attribute. See {!Irmin_git.config}.*)

  module AO (G: Git.Store.S): Irmin.AO_MAKER
  (** Embed an append-only store into a Git repository. Contents will
      be written in {i .git/objects/} and might be cleaned-up if you
      run {i git gc} manually. *)

  module RW (G: Git.Store.S) (K: Irmin.Tag.S) (V: Irmin.Hash.S): Irmin.RW
    with type key = K.t and type value = V.t
  (** Embed a read-write store into a Git repository. Contents will be
      written in {i .git/refs}. *)

  module Memory: Irmin.S_MAKER
  (** Embed an Irmin store into an in-memory Git repository. *)

  module FS: Irmin.S_MAKER
  (** Embed an Irmin store into a local Git repository. *)

end

(** REST (over HTTP) backend.. *)
module Irmin_http: sig

  (** {1 HTTP client} *)

  val config: Uri.t -> Irmin.config
  (** Create a configuration value. [uri] it the location of the
      remote HTTP {{!module:Irmin_http_server}server}. *)

  val uri: Uri.t option Irmin.Private.Conf.key
  (** The configuration key to set the location of the remote HTTP
      {{!module:Irmin_http_server}server}. *)

  module AO: Irmin.AO_MAKER
  (** An HTTP client using a REST API for an append-only store. *)

  module RW: Irmin.RW_MAKER
  (** An HTTP client using a REST API for a read-write store. *)

  module Make: Irmin.S_MAKER
  (** [Make] provides high-level bindings to the remote HTTP server.

      Most of the computation are done on the server, the client is
      (almost) stateless. The only thing that the client needs to
      remember is the tag of the current branch or the current head if
      the branch is detached.

      All of the low-level and high-level operations take only one
      RTT. *)

  module Low: Irmin.S_MAKER
  (** [Low] provides low-level bindings to the remote HTTP server.

      Only the {{!Irmin.S.Private}low-level operations} are forwarded
      to the server, all the high-level logic is done on the
      client. Hence a high-level operation might take multiple
      RTTs. *)

end

(** Server-side of the REST API over HTTP. *)
module Irmin_http_server: sig

  (** {1 HTTP server} *)

  type hooks = {
    update: unit -> unit Lwt.t;
  }
  (** Server hooks. *)

  (** Server Signature. *)
  module type S = sig

    type t
    (** The type for store handles. *)

    val listen: t -> ?timeout:int -> ?hooks:hooks -> Uri.t -> unit Lwt.t
    (** [start_server t uri] start a server serving the contents of
        [t] at the address [uri]. Close clients' connections after
        [timeout] seconds of inactivity. *)

  end

  module Make (S: Irmin.S): S with type t = S.t
  (** [Make] exposes an Irmin store as a REST API for HTTP
      {{!module:Irmin_http}clients}. *)

end

val install_dir_polling_listener: float -> unit
(** Install the directory listener using active polling. The parameter
    is the thread sleep time. Prefer
    {{:https://opam.ocaml.org/packages/inotify/inotify.2.0/}inotify}
    if it works on your system. *)

module type LOCK = sig

  (** {1 Filesystem {i dotlocking}} *)

  val with_lock: string -> (unit -> 'a Lwt.t) -> 'a Lwt.t
  (** [with_lock file fn] runs [fn] while holding a lock on the file
      [file]. *)

end

module Lock: LOCK
(** An implementation of filesystem dotlocking. *)
