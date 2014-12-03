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

(** Git backend *)

val config: ?root:string -> ?bare:bool -> (module Git.Store.S) -> Irmin.config

module AO (G: Git.Store.S): Irmin.AO with type value = Cstruct.t
module RW (G: Git.Store.S): Irmin.RW with type key = string list

module type S = Irmin.S with type step = string and type tag = string list

module Memory (IO: Git.Sync.IO) (C: Irmin.Contents.S):
  S with type value = C.t

module FS (IO: Git.Sync.IO) (G: Git.FS.IO) (C: Irmin.Contents.S):
  S with type value = C.t
