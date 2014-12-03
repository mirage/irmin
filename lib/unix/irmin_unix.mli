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

module Irmin_fs: sig

  (** {1 File-system store} *)

  val config: path:string -> Irmin.config

  module AO: Irmin.AO_MAKER
  module RW: Irmin.RW_MAKER
  module Make: Irmin.S_MAKER

  (** {1 Extended configuration} *)

  module type Config = Irmin_fs.Config

  module AO_ext (C: Config): Irmin.AO_MAKER
  module RW_ext (C: Config): Irmin.RW_MAKER
  module Make_ext (Obj: Config) (Ref: Config): Irmin.S_MAKER

end

module Irmin_git: sig

  val config: ?root:string -> ?bare:bool -> (module Git.Store.S) -> Irmin.config

  module AO (G: Git.Store.S): Irmin.AO with type value = Cstruct.t
  module RW (G: Git.Store.S): Irmin.RW with type key = string list

  module type S = Irmin.S with type step = string and type tag = string list

  module Memory (C: Irmin.Contents.S):
    S with type value = C.t

  module FS (C: Irmin.Contents.S):
    S with type value = C.t

end

module Irmin_http: sig

  (** {1 HTTP client} *)

  val config: Uri.t -> Irmin.config

  module Make: Irmin.S_MAKER
  module Low: Irmin.S_MAKER

end

module Irmin_http_server: sig

  (** {1 HTTP server} *)

  module type S = Irmin_http_server.S

  module Make (S: Irmin.S): S with type t = S.t

end

val install_dir_polling_listener: float -> unit
(** Install the directory listener using active polling. The
    parameter is the thread sleep time. *)
