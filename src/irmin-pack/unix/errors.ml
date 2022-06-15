(*
 * Copyright (c) 2022-2022 Tarides <contact@tarides.com>
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

include Errors_base

module type S = sig
  module Io : Io.S

  val pp_error :
    Format.formatter -> [< error | `Io_misc of Io.misc_error ] -> unit

  val raise_error : [< error | `Io_misc of Io.misc_error ] -> 'a
  val raise_if_error : ('a, [< error | `Io_misc of Io.misc_error ]) result -> 'a
end

module Make (Io : Io.S) : S with module Io = Io = struct
  module Io = Io

  type misc_error = Io.misc_error [@@deriving irmin ~pp]

  let pp_error ppf = function
    | `Io_misc e -> Format.fprintf ppf "%a" pp_misc_error e
    | #error as e -> Format.fprintf ppf "%a" pp_error e

  let raise_error = function
    | `Io_misc e -> Io.raise_misc_error e
    | `Ro_not_allowed -> raise RO_not_allowed
    | #error' as e -> raise (Io_error e)

  let raise_if_error = function Ok x -> x | Error e -> raise_error e
end
