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

module Memory (S: Git.Sync.S) (C: Irmin.Contents.S): sig
  include Irmin.S with type step = string
                   and type tag = string
                   and type value = C.t
  val create: ?root:string -> Irmin.Task.t -> t
  val of_tag: ?root:string -> Irmin.Task.t -> tag -> t
  val of_head: ?root:string -> Irmin.Task.t -> head -> t
end

module Make (G: Git.Store.S) (S: Git.Sync.S) (C: Irmin.Contents.S): sig
  include Irmin.S with type step = string
                   and type tag = string
                   and type value = C.t
  val create: ?root:string -> ?bare:bool -> Irmin.Task.t -> t
  val of_tag: ?root:string -> ?bare:bool -> tag -> Irmin.Task.t -> t
  val of_head: ?root:string -> ?bare:bool -> head -> Irmin.Task.t -> t
end
