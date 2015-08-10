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

(** Git backend *)

(* Discard the hash implementation passed in parameter of the functors. *)

val config:
  ?root:string -> ?head:Git.Reference.t -> ?bare:bool -> unit -> Irmin.config

val bare: bool Irmin.Private.Conf.key
val head: Git.Reference.t option Irmin.Private.Conf.key

module type LOCK = sig
  val with_lock: string -> (unit -> 'a Lwt.t) -> 'a Lwt.t
end

module AO (G: Git.Store.S): Irmin.AO_MAKER

module RW (L: LOCK) (G: Git.Store.S) (K: Irmin.Tag.S) (V: Irmin.Hash.S):
  Irmin.RW with type key = K.t and type value = V.t

module Memory (IO: Git.Sync.IO) (I: Git.Inflate.S):
  Irmin.S_MAKER

module FS (IO: Git.Sync.IO) (I: Git.Inflate.S) (L: LOCK) (FS: Git.FS.IO):
  Irmin.S_MAKER

module type CONTEXT = sig type t val v: unit -> t option Lwt.t end

module Memory_ext (C: CONTEXT)
    (IO: Git.Sync.IO with type ctx = C.t) (I: Git.Inflate.S):
  Irmin.S_MAKER

module Internals (S: Irmin.S): sig

  (** {1 Access to the Git objects} *)

  val commit_of_head: S.t -> S.head -> Git.Commit.t option Lwt.t
  (** [commit_of_head t h] is the commit corresponding to [h] in the
      store [t]. *)

end
