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

(** {1 Unix backends}

    This module provides Irmin backends for Unix applications:

    {ul
    {- A {{!Irmin_fs}file-system} backend, using
    {{:https://github.com/janestreet/bin_prot}bin_prot} to serialize
    internal structures.}
    {- A fully compatible, bi-directional encoding of Irmin into
    {{!Irmin_git}Git}. You can view and edit your store using both the
    library and your usual Git tools. }
    {- {!Irmin_http} and {!Irmin_http_server} FIXME }
    {- {!Irmin_mem} FIXME }
    }
*)

val task: string -> Irmin.task
(** [task fmt] creates a fresh task, with the {{!Irmin.Task.date}date}
    set with [Unix.gettimeoday] and a proper
    {{!Irmin.Task.owner}owner}. *)

(** File system backends, using
    {{:https://github.com/janestreet/bin_prot}bin_prot}. *)
module Irmin_fs: sig

  (** {1 File-system Store} *)

  val config: ?root:string -> unit -> Irmin.config
  (** Create a configuration value. *)

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

(** Git backends. *)
module Irmin_git: sig

  (** {1 Git Store} *)

  (** FIXME *)

  val config: ?root:string -> ?branch:string -> ?bare:bool -> unit -> Irmin.config

  val branch: string Irmin.Private.Conf.key
  val bare: bool Irmin.Private.Conf.key

  module AO (G: Git.Store.S): Irmin.AO_MAKER
  module RW (G: Git.Store.S): Irmin.RW_MAKER

  module Memory: Irmin.S_MAKER
  module FS: Irmin.S_MAKER
end

(** REST (over HTTP) backend.. *)
module Irmin_http: sig

  (** {1 HTTP client} *)

  (** FIXME *)

  val config: Uri.t -> Irmin.config
  val uri: Uri.t option Irmin.Private.Conf.key

  module AO: Irmin.AO_MAKER
  module RW: Irmin.RW_MAKER

  module Make: Irmin.S_MAKER
  (** High-level bindings. Most of the computation is done on the
      server, the client is (almost) stateless. The only thing that
      the client needs to remember is the tag of the current branch or
      the current head if the branch is detached. *)

  module Low: Irmin.S_MAKER
  (** Low-level bindings. Only access the backend stores, all the
      high-level logic is done on the client. *)

end

(** Server-side of the REST API over HTTP. *)
module Irmin_http_server: sig

  (** {1 HTTP server} *)

  (** FIXME *)

  module type S = sig

    type t
    (** Database type. *)

    val listen: t -> ?timeout:int -> Uri.t -> unit Lwt.t
    (** [start_server t uri] start a server serving the contents of [t]
        at the address [uri]. *)

  end

  module Make (S: Irmin.S): S with type t = S.t

end

val install_dir_polling_listener: float -> unit
(** Install the directory listener using active polling. The
    parameter is the thread sleep time. *)
