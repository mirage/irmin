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

let src = Logs.Src.create "irmin-pack.unix" ~doc:"irmin-pack unix backend"

module Log = (val Logs.src_log src : Logs.LOG)

module Array = struct
  include Array

  let find_opt p a =
    let n = length a in
    let rec loop i =
      if i = n then None
      else
        let x = get a i in
        if p x then Some x else loop (succ i)
    in
    loop 0
end

module List = struct
  include List

  let rec iter_result f = function
    | [] -> Ok ()
    | hd :: tl -> Result.bind (f hd) (fun () -> iter_result f tl)
end

module Int63 = struct
  include Optint.Int63

  let t = Irmin.Type.int63

  module Syntax = struct
    let ( + ) = add
    let ( - ) = sub
    let ( * ) = mul
    let ( / ) = div
    let ( < ) a b = compare a b < 0
    let ( <= ) a b = compare a b <= 0
    let ( > ) a b = compare a b > 0
    let ( >= ) a b = compare a b >= 0
    let ( = ) = equal
  end
end

type int63 = Int63.t [@@deriving irmin]

module Version = Irmin_pack.Version

module type S = Irmin_pack.S

module Conf = Irmin_pack.Conf
module Layout = Irmin_pack.Layout
module Indexable = Irmin_pack.Indexable

module Result_syntax = struct
  let ( let+ ) res f = Result.map f res
  let ( let* ) = Result.bind
  let ( >>= ) = Result.bind
end

module Varint = struct
  type t = int [@@deriving irmin ~decode_bin]

  (** LEB128 stores 7 bits per byte. An OCaml [int] has at most 63 bits.
      [63 / 7] equals [9]. *)
  let max_encoded_size = 9
end
