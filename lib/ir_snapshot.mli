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

(** Manage snapshot/revert capabilities. *)

module type OF_STORE = sig

  (** Snapshots are read-only checkpoints of the dabase. *)

  include Ir_ro.STORE

  type db
  (** Type for database handler. *)

  val create: db -> origin -> t Lwt.t
  (** Snapshot the current state of the store. *)

  val revert: db -> origin -> t -> unit Lwt.t
  (** Revert the store to a previous state. *)

  val merge: db -> origin -> t -> unit Ir_merge.result Lwt.t
  (** Merge the given snasphot into the current branch of the
      database. *)

  val watch: db -> origin -> key -> (key * t) Lwt_stream.t
  (** Subscribe to the stream of modification events attached to a
      given path. Takes and returns a new snapshot every time a
      sub-path is modified. *)

  type state
  (** Type for snapshot stats. *)

  val of_state: db -> state -> t
  (** Create a snapshot from a database state. *)

  val to_state: t -> state
  (** Get the databae state. *)

end

module Of_store (S: Ir_bc.STORE_EXT):
  OF_STORE with type db = S.t
            and type state = S.Block.Node.key
            and type origin = S.origin
            and type key = S.key
(** Add snapshot capabilities to a branch-consistent store. *)
