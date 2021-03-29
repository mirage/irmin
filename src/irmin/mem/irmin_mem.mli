(*
 * Copyright (c) 2013-2017 Thomas Gazagnaire <thomas@gazagnaire.org>
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

(** In-memory store.

    A simple in-memory store, using hash tables. Once one of the functors below
    is instantiated to a module [M], it has a unique shared hash-table: multiple
    invocation of [M.create] will see and manipulate the same contents. *)

val config : unit -> Irmin.config
(** Configuration values. *)

module Append_only : Irmin.Append_only_store_maker
(** An in-memory store for append-only values. *)

module Atomic_write : Irmin.Atomic_write_store_maker
(** An in-memory store with atomic-write guarantees. *)

module Make : Irmin.Maker
(** Constructor for in-memory Irmin store. *)

(** Constructor for in-memory KV stores. Subtype of {!Irmin.KV_maker}. *)
module KV (C : Irmin.Contents.S) :
  Irmin.KV
    with type contents = C.t
     and type metadata = unit
     and type Private.Remote.endpoint = unit
