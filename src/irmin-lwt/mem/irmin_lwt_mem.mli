(*
 * Copyright (c) 2026 Tarides
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

(** In-memory backend for [irmin-lwt].

    A thin Lwt-flavoured shim over Irmin 4's in-memory backend. Each Lwt-typed
    operation forwards to its Irmin 4 counterpart through {!Lwt_eio.run_eio}.
    Apart from the Lwt return types, the API mirrors [Irmin_mem]. *)

module Conf : sig
  val spec : Irmin_lwt.Backend.Conf.Spec.t
end

val config : unit -> Irmin_lwt.config
(** Configuration values. *)

module Append_only : Irmin_lwt.Append_only.Maker
(** An in-memory store for append-only values. *)

module Content_addressable : Irmin_lwt.Content_addressable.Maker
(** An in-memory store for content-addressable values. *)

module Atomic_write : Irmin_lwt.Atomic_write.Maker
(** An in-memory store with atomic-write guarantees. *)

(** Constructor for in-memory KV stores. *)
module KV :
  Irmin_lwt.KV_maker
    with type endpoint = unit
     and type metadata = unit
     and type info = Irmin_lwt.Info.default

include Irmin_lwt.Maker with type endpoint = unit
(** Constructor for in-memory Irmin store. *)
