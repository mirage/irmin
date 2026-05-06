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

(** On-disk filesystem backend for [irmin-lwt].

    A thin Lwt-flavoured shim over Irmin 4's [irmin-fs.unix] backend. Each
    Lwt-typed operation forwards to its Irmin 4 counterpart through
    {!Lwt_eio.run_eio}. The configuration leaks Eio types: [config] takes
    [_ Eio.Path.t] for the root directory and [_ Eio.Time.clock] (used for file
    lock staleness detection); Lwt callers obtain these from their
    [Eio_main.run] runner. See LIMITATIONS.md. *)

val config :
  root:_ Eio.Path.t -> clock:_ Eio.Time.clock -> Irmin_lwt.Backend.Conf.t
(** [config ~root ~clock] is a configuration with the root directory and the
    clock set. *)

val spec :
  path:_ Eio.Path.t -> clock:_ Eio.Time.clock -> Irmin_lwt.Backend.Conf.Spec.t

module Append_only : Irmin_lwt.Append_only.Maker
(** Append-only store on top of the filesystem. *)

module Atomic_write : Irmin_lwt.Atomic_write.Maker
(** Atomic-write store on top of the filesystem (with file locking for
    [test_and_set]). *)

module Content_addressable : Irmin_lwt.Content_addressable.Maker
(** Content-addressable store derived from {!Append_only}. *)

(** Constructor for filesystem KV stores. *)
module KV :
  Irmin_lwt.KV_maker
    with type endpoint = unit
     and type metadata = unit
     and type info = Irmin_lwt.Info.default

include Irmin_lwt.Maker with type endpoint = unit
(** Constructor for filesystem-backed Irmin stores. *)
