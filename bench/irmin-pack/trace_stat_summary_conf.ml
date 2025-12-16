(*
 * Copyright (c) 2018-2022 Tarides <contact@tarides.com>
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

(** Summary configuration.

    This file is NOT meant to be used from Tezos, as opposed to some other
    "trace_*" files. *)

let histo_bin_count = 16
let curves_sample_count = 201

(** Parameter to control the width of the moving average window.

    This width is expressed as a fraction of the width of the final plot images,
    (i.e. the number of played blocks). This implies that the amount of
    smoothing remains visually the same, no matter [curves_sample_count] and the
    number of blocks in the stat trace.

    Justification of the current value:

    The 2nd commit of the full replay trace is a massive one, it contains ~1000x
    more operations than an average one, it takes ~10 half lives to fully get
    rid of it from the EMAs. With [moving_average_half_life_ratio = 1. /. 80.],
    that 2nd commit will only poluate [1 / 8] of the width of the smoothed
    curves. *)
let moving_average_half_life_ratio = 1. /. 80.

(** See [Exponential_moving_average] *)
let moving_average_relevance_threshold = 0.999
