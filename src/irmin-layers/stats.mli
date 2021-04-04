(*
 * Copyright (c) 2018-2021 Tarides <contact@tarides.com>
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

(** {2 Profiling of Freeze}

    {3 Control Flow of the Freeze Thread}

    Most of the functions here should be called from under a single mutex lock. *)

val freeze_start : Mtime.t -> string -> unit
(** Signals the start of a new freeze given the time at which the freeze process
    started and the name given to the initial code section. *)

val freeze_section : string -> unit
(** Signals that the freeze is entering a specific section of the code. *)

val freeze_stop : unit -> unit
(** Signals the end of an ongoing freeze freeze. *)

val freeze_yield : unit -> unit
(** Signals that the freeze is cooperatively yielding to other threads. *)

val freeze_yield_end : unit -> unit
(** Signals that the freeze is given back the control. *)

(** {3 Incrementations of Counters} *)

val copy_contents : unit -> unit
(** Increments the number of copied contents for the current freeze. *)

val copy_nodes : unit -> unit
(** Increments the number of copied nodes for the current freeze. *)

val copy_commits : unit -> unit
(** Increments the number of copied commits for the current freeze. *)

val copy_branches : unit -> unit
(** Increments the number of copied branches for the current freeze. *)

val add : unit -> unit
(** Increment the number of objects added by main thread. *)

val skip_test : bool -> unit
(** Increment the number time we wondered if an entry was present at the
    destination during a graph traversal for the current freeze. *)

val copy_newies_loop : unit -> unit
(** Increment the number of iterations of newies copy. *)

(** {3 Observation} *)

val get_add_count : unit -> int
val get_copied_commits_count : unit -> int
val get_copied_branches_count : unit -> int
val get_copied_contents_count : unit -> int
val get_copied_nodes_count : unit -> int
val get_freeze_count : unit -> int
val pp_latest : Format.formatter -> unit

(** {3 Misc.} *)

val reset_stats : unit -> unit
