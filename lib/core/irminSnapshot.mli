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

open IrminSig

module type STORE = sig

  (** A store with snapshot/revert capabilities. *)

  type t
  (** Database handler. *)

  type path
  (** Database paths. *)

  type state
  (** Snapshot states. *)

  val create: t -> state Lwt.t
  (** Snapshot the current state of the store. *)

  val revert: t -> state -> unit Lwt.t
  (** Revert the store to a previous state. *)

  val merge: t -> ?origin:origin -> state -> unit IrminMerge.result Lwt.t
  (** Merge the given snasphot into the current branch of the
      database. *)

  val merge_exn: t -> ?origin:origin -> state -> unit Lwt.t
  (** Same as [merge_snapshot] but raise a [Conflict] exception in
      case of conflict. *)

  val watch: t -> path -> (path * state) Lwt_stream.t
  (** Subscribe to the stream of modification events attached to a
      given path. Return an event for each modification of a
      subpath. *)

end

module Make (S: IrminBranch.STORE): STORE with type t = S.t
                                           and type state = S.Block.key
(** Add snapshot capabilities to a branch-consistent store. *)
