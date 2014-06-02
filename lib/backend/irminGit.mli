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

(** Serialize the irminsule objects to a local Git store. *)

module type Config = sig

  val root: string option
  (** Database root. *)

  module Store: Git.Store.S
  (** Git database implementation. Can be [Git_fs] or [Git_memory]. *)

  val bare: bool
  (** Should we extend the filesystem *)

  val disk: bool
  (** Enable disk operations such as installing watches and limiting
      concurrent open files. Should be consistent with the [Store]
      implementation. *)

end

module Memory: Config
(** In-memory Git store (using [Git_memory]). *)

module Make (C: Config): Irmin.BACKEND

val connect: (module Config) -> ?depth:int -> Git.Gri.t -> unit Lwt.t
(** Connect to a remote store. *)
