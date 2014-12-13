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

(** {1 Unix backends} *)

val task: string -> Irmin.task
(** [task fmt] creates a fresh task, with the {{!Irmin.Task.date}date}
    set with [Unix.gettimeoday] and a proper
    {{!Irmin.Task.owner}owner}. *)

(** File system backends. *)
module Irmin_fs: sig

  (** {1 File-system store} *)

  val config: ?root:string -> unit -> Irmin.config

  module AO: Irmin.AO_MAKER
  module RW: Irmin.RW_MAKER
  module Make: Irmin.S_MAKER

  (** {1 Extended configuration} *)

  module type Config = Irmin_fs.Config

  module AO_ext (C: Config): Irmin.AO_MAKER
  module RW_ext (C: Config): Irmin.RW_MAKER
  module Make_ext (Obj: Config) (Ref: Config): Irmin.S_MAKER

end

(** Git backends. *)
module Irmin_git: sig

  val config: ?root:string -> ?branch:string -> ?bare:bool -> unit -> Irmin.config

  val branch: string Irmin.Conf.key
  val bare: bool Irmin.Conf.key

  module AO (G: Git.Store.S): Irmin.AO_MAKER
  module RW (G: Git.Store.S): Irmin.RW_MAKER

  module Memory: Irmin.S_MAKER
  module FS: Irmin.S_MAKER
end

(** REST (over HTTP) backend.. *)
module Irmin_http: sig

  (** {1 HTTP client} *)

  val config: Uri.t -> Irmin.config
  val uri: Uri.t option Irmin.Conf.key

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

  module type S = Irmin_http_server.S

  module Make (S: Irmin.S): S with type t = S.t

end

val install_dir_polling_listener: float -> unit
(** Install the directory listener using active polling. The
    parameter is the thread sleep time. *)
