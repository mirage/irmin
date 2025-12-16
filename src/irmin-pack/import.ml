(*
 * Copyright (c)2018-2022 Tarides <contact@tarides.com>
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

include Irmin.Export_for_backends

let src = Logs.Src.create "irmin.pack" ~doc:"irmin-pack backend"

module Log = (val Logs.src_log src : Logs.LOG)

module Int63 = struct
  include Optint.Int63

  let t = Irmin.Type.int63
end

type int63 = Int63.t [@@deriving irmin]

module Mem = struct
  let bytes_per_word = Sys.word_size / 8

  let reachable_bytes o =
    Obj.repr o |> Obj.reachable_words |> Int.mul bytes_per_word

  let repr_size : type a. a Repr.t -> a -> int =
   fun ty ->
    match Irmin.Type.Size.of_value ty with
    | Unknown -> Fun.const max_int
    | Dynamic f -> fun v -> f v
    | Static n -> Fun.const n
end
