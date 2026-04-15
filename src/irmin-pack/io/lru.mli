(*
 * Copyright (c) 2023 Tarides <contact@tarides.com>
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

open Import

type t
(** An LRU that support memory-bound capacity via configuration key
    [lru_max_memory]. Falls back to entry-based capacity via [lru_size]
    configuration key, if max memory is not configured. *)

type key = int63
type value = Irmin_pack.Pack_value.kinded

val create : Irmin.Backend.Conf.t -> t

val add : t -> int63 -> Irmin_pack.Pack_value.weight -> value -> unit
(** [add t key weight value] maps [value] with [weight] to [key] in [t].

    Note: {!Irmin_pack.Pack_value.Immediate} weights will be checked to see if
    they exceed an entry weight limit. The current hard-coded entry weight limit
    is 20kB. *)

val find : t -> key -> value
val mem : t -> key -> bool
val clear : t -> unit
val iter : t -> (key -> value -> unit) -> unit
