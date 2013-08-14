(*
 * Copyright (c) 2013 Thomas Gazagnaire <thomas@gazagnaire.org>
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

(** Compose IO operations *)

open IrminTypes

(** Lift IO operation to lists *)
module List
    (C: CHANNEL)
    (E: IO with type channel = C.t):
  IO with type t = E.t list
      and type channel = C.t

(** Lift IO operation to options *)
module Option
    (C: CHANNEL)
    (E: IO with type channel = C.t):
  IO with type t = E.t option
      and type channel = C.t


(** Lift IO operations to pairs *)
module Pair
    (C: CHANNEL)
    (K: IO with type channel = C.t)
    (V: IO with type channel = C.t):
  IO with type t = K.t * V.t
      and type channel = C.t

(** serialization to strings *)
module type STRINGABLE = sig
  type t
  val to_string: t -> string
  val of_string: string -> t
end

(** Lift IO operations to strings *)
module String (C: CHANNEL) (S: STRINGABLE):
  IO with type t = S.t
      and type channel = C.t
