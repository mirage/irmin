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

(** Management of disk-format versions.

    [`V2] introduced the [*_v2] kinds and deprecated the [*_v1] ones. The
    upgrade of a pack file to [`V2] was done silently the first time a used
    pushed a [*_v2] entry to the pack file.

    [`V3] introduced the control file. It centralizes all the metadata that used
    to be contained in the header of other files (e.g. the version of the store
    used to be stored in each files, it is now solely stored in the control
    file). The upgrade of a store to [`V3] was done silently when opening a
    store in rw mode.

    [`V4] introduced the chunked suffix. Upgrade from [`V3] happened on first
    write to the control file.

    [`V5] introduced the lower layer. Upgrade happened on first write to the
    control file. *)

type t = [ `V1 | `V2 | `V3 | `V4 | `V5 ] [@@deriving irmin]
(** The type for version numbers. *)

val to_int : t -> int
val compare : t -> t -> int
val latest : t

val pp : t Fmt.t
(** [pp] is the pretty-format for version numbers. *)

val to_bin : t -> string
(** [to_bin t] is the 8-bytes binary representation of [t]. *)

val of_bin : string -> t option
(** [of_bin s] is [Some t] is [to_bin t] is [s] and [None] otherwise. *)

val invalid_arg : string -> 'a
(** [invalid_arg str] raises [Invalid_argument]. *)

exception Invalid of { expected : t; found : t }
