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

    A simple in-memory store, using hash tables. Once one of the
    functors below is instantiated to a module [M], it has a unique
    shared hash-table: multiple invocation of [M.create] will see and
    manipulate the same contents. *)

val config: unit -> Irmin.config
(** Configuration values. *)

module AO: Irmin.AO_MAKER
(** An in-memory append-only store. *)

module Link: Irmin.LINK_MAKER
(** Immutable links. *)

module RW: Irmin.RW_MAKER
(** An in-memory read-write store. *)

module Make: Irmin.S_MAKER
(** An in-memory Irmin store. *)

module KV: Irmin.KV_MAKER
(** An in-memory KV-store. *)
