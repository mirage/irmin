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

(** {1 Commit Info} *)

type t
(** The type for commit info. *)

val v : date:int64 -> author:string -> string -> t
(** Create a new commit info. *)

val date : t -> int64
(** [date t] is [t]'s commit date.

    The date provided by the user when calling the {{!Info.v} create} function.
    Rounding [Unix.gettimeofday ()] (when available) is a good value for such
    date. On more esoteric platforms, any monotonic counter is a fine value as
    well. On the Git backend, the date is translated into the commit {e Date}
    field and is expected to be the number of POSIX seconds (thus not counting
    leap seconds) since the Epoch. *)

val author : t -> string
(** [author t] is [t]'s commit author.

    The author identifies the entity (human, unikernel, process, thread, etc)
    performing an operation. For the Git backend, this will be directly
    translated into the {e Author} field. *)

val message : t -> string
(** [message t] is [t]'s commit message. *)

val empty : t
(** The empty commit info. *)

(** {1 Info Functions} *)

type f = unit -> t
(** Alias for functions which can build commit info. *)

val none : f
(** The empty info function. [none ()] is [empty] *)

(** {1 Value Types} *)

val t : t Type.t
(** [t] is the value type for {!t}. *)
