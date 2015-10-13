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

(** Store Synchronisation. *)

type remote
val remote_uri: string -> remote
val remote_store: (module Ir_s.STORE_EXT with type t = 'a) -> 'a -> remote

module type STORE = sig
  type db
  type commit_id
  val fetch: db -> ?depth:int -> remote ->
    [`Head of commit_id | `No_head | `Error] Lwt.t
  val fetch_exn: db -> ?depth:int -> remote -> commit_id Lwt.t
  val pull: db -> ?depth:int -> remote -> [`Merge|`Update] ->
    [`Ok | `No_head | `Error] Ir_merge.result Lwt.t
  val pull_exn: db -> ?depth:int -> remote -> [`Merge|`Update] -> unit Lwt.t
  val push: db -> ?depth:int -> remote -> [`Ok | `Error] Lwt.t
  val push_exn: db -> ?depth:int -> remote -> unit Lwt.t
end

module Make (S: Ir_s.STORE_EXT): STORE
  with type db = S.t and type commit_id = S.commit_id
