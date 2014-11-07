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

module type S = sig

  (** Snapshots are read-only checkpoints of the dabase. *)

  include Ir_rw.STORE

  type db
  (** Type for database handler. *)

  val create: db -> t Lwt.t
  (** Snapshot the current state of the store. *)

  val revert: db -> t -> unit Lwt.t
  (** Revert the store to a previous state. *)

  val merge: db -> ?origin:origin -> t -> unit Ir_merge.result Lwt.t
  (** Merge the given snasphot into the current branch of the
      database. *)

  val merge_exn: db -> ?origin:origin -> t -> unit Lwt.t
  (** Same as [merge_snapshot] but raise a [Conflict] exception in
      case of conflict. *)

  val watch: db -> key -> (key * t) Lwt_stream.t
  (** Subscribe to the stream of modification events attached to a
      given path. Takes and returns a new snapshot every time a
      sub-path is modified. *)

  type state
  (** Snapshot state. *)

  val of_state: db -> state -> t
  (** Create a snapshot from a state. *)

  val to_state: t -> state
  (** Get the snapshot state. *)

  include Tc.I0 with type t := state

end

module Make (S: Ir_bc.MAKER_EXT): STORE with type state = Block.key
(** Add snapshot capabilities to a branch-consistent store. *)
