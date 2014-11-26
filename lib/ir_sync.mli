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

(** Store Synchronisation. *)

module type S = sig
  type db
  type head
  type remote
  val uri: string -> remote
  val store: db -> remote
  val fetch: db -> ?depth:int -> remote -> head option Lwt.t
  val fetch_exn: db -> ?depth:int -> remote -> head Lwt.t
  val push: db -> ?depth:int -> remote -> head option Lwt.t
  val push_exn: db -> ?depth:int -> remote -> head Lwt.t
end

module type REMOTE = functor (S: Ir_bc.STORE) -> sig
  val fetch: S.t -> ?depth:int -> string -> S.head option Lwt.t
  val push : S.t -> ?depth:int -> string -> S.head option Lwt.t
end

module None: REMOTE
(** An implementation of remote synchronisation which does nothing,
    i.e. it always return [None] on [fetch] and [push]. *)

module Make (S: Ir_bc.STORE) (R: REMOTE):
  S with type db = S.t and type head = S.head
(** Use [R] to synchronize stores using some native (and usually fast)
    backend-specific protocols. *)
