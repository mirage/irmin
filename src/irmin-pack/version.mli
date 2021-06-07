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

(** Management of disk-format versions. *)

type t = [ `V1 | `V2 ]
(** The type for version numbers. *)

val pp : t Fmt.t
(** [pp] is the pretty-format for version numbers. *)

val to_bin : t -> string
(** [to_bin t] is the 8-bytes binary representation of [t]. *)

val of_bin : string -> t option
(** [of_bin s] is [Some t] is [to_bin t] is [s] and [None] otherwise. *)

val invalid_arg : string -> 'a
(** [invalid_arg str] raises [Invalid_argument]. *)

exception Invalid of { expected : t; found : t }

module type S = sig
  val version : t
end

module V1 : S
module V2 : S
