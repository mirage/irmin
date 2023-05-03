(*
 * Copyright (c) 2022-2023 Tarides <contact@tarides.com>
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

open! Import

type t
(** An ordered set of disjoint [(offset, length)] ranges. *)

val make : unit -> t
(** [make ()] returns a new empty set of ranges. *)

val add : off:int63 -> len:int -> t -> unit
(** [add ~off ~len t] inserts the range [(off, len)] into [t]. When [add] is
    called multiple times sequentially, it is optimized for strictly decreasing
    offsets arguments. *)

val iter : (off:int63 -> len:int63 -> unit) -> t -> unit
(** [iter fn t] calls [fn ~off ~len] on every disjoint range [(off, len)] in the
    set [t]. The function [fn ~off ~len] is called with strictly increasing
    offsets. If two or more consecutive ranges [(off,len)] and [(off+len,len')]
    were added to the set [t], a single call to [fn] will be performed on the
    englobing interval [(off,len+len')]. *)

val count : t -> int
(** [count t] returns the number of [add]s performed on [t]. *)
