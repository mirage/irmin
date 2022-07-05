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

(** This module defines abstractions over entries in the pack file, which are
    encoded as follows:

    {v
      ┌────────┬────────┬──────────────┬─────────┐
      │  Hash  │  Kind  │  Len(Value)? │  Value  │
      └────────┴────────┴──────────────┴─────────┘
      ┆<┄ H ┄┄>┆<┄ K ┄┄>┆<┄┄┄┄ L? ┄┄┄┄>┆<┄┄ V ┄┄>┆
      ┆<┄┄┄┄┄┄┄┄┄┄┄ entry length, E ┄┄┄┄┄┄┄┄┄┄┄┄>┆
    v}

    The fields are as follows:

    - [Hash]: the (fixed-length) hash of the data stored in this entry;

    - [Kind]: the {i kind} of data being stored (contents, nodes, commits etc.),
      encoded as a single "magic" character;

    - [Len(Value)]: an optional length header for the [Value] section of the
      entry ({i not} including the length of the length header itself), encoded
      using a variable-length encoding (LEB128). The presence of a length header
      is determined by the [Kind] character.

    - [Value]: the data itself.

    The length of the overall pack {i entry}, as referenced in the {!Pack_index}
    or in a direct {!Pack_key.t}, is equal to [E = H + K + L + V]. *)

include Pack_value_intf.Sigs
(** @inline *)
