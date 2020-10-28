(*
 * Copyright (c) 2013-2020 Ioana Cristescu <ioana@tarides.com>
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

type t = {
  mutable nb_freeze : int;
  mutable copied_contents : int list;
  mutable copied_nodes : int list;
  mutable copied_commits : int list;
  mutable copied_branches : int list;
  mutable waiting_freeze : float list;
  mutable completed_freeze : float list;
  mutable skips : int;
}
(** The type for stats for a store S.

    - [nb_freeze] is the number of calls to [S.freeze];
    - [copied_contents] is the number of contents copied per freeze;
    - [copied_nodes] is the number of nodes copied per freeze;
    - [copied_commits] is the number of commits copied per freeze;
    - [copied_branches] is the number of branches copied per freeze;
    - [waiting_freeze] are the times the freeze threads waited to start, for the
      last 10 freezes at most;
    - [completed_freeze] are the times the freeze threads took to complete, for
      the last 10 freezes at most;
    - [skips] the number of skips during the graph traversals called by the
      freeze, where the same object can be skipped several times;

    Note that stats assume that there always is an ongoing freeze. If its not
    the case, you should discard the last 0.*)

val reset_stats : unit -> unit

val get : unit -> t

val copy_contents : unit -> unit
(** Increments t.copied_contents for the current freeze *)

val copy_nodes : unit -> unit
(** Increments t.copied_nodes for the current freeze *)

val copy_commits : unit -> unit
(** Increments t.copied_commits for the current freeze *)

val copy_branches : unit -> unit
(** Increments t.copied_branches for the current freeze *)

val freeze : unit -> unit
(** Signals the start of a new freeze, and increments t.nb_freeze *)

val add : unit -> unit
(** Increment the number of objects added, do not call this for objects copied
    during a freeze. *)

val get_adds : unit -> int
(** Get the number of objects added, not including the copied objects during a
    freeze. *)

val reset_adds : unit -> unit
(** Reset the number of objects added. *)

val with_timer : [ `Freeze | `Waiting ] -> (unit -> unit Lwt.t) -> unit Lwt.t
(** Either the duration of a freeze thread waiting on the freeze lock to start;
    or the duration of a freeze thread to complete a freeze. *)

val skip : unit -> unit
(** Increment the number of skips during a graph traversal. *)
