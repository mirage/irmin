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

module Pack_value = Irmin_pack.Pack_value
module Version = Irmin_pack.Version

module type S = Irmin_pack.S

module Conf = Irmin_pack.Conf
module Layout = Irmin_pack.Layout
module Pack_key = Irmin_pack.Pack_key
module Stats = Stats
module Indexable = Irmin_pack.Indexable

module Result_syntax = struct
  let ( let+ ) res f = Result.map f res
  let ( let* ) res f = Result.bind res f
end

type int_bigarray = (int, Bigarray.int_elt, Bigarray.c_layout) Bigarray.Array1.t
(** [int_bigarray] is the raw type for the mapping file data, exposed via mmap *)

module BigArr1 = Bigarray.Array1
(** Simple module alias *)

(** Essentially the Y combinator; useful for anonymous recursive functions. The
    k argument is the recursive call. Example:

    {[
      iter_k (fun ~k n -> if n = 0 then 1 else n * k (n - 1))
    ]} *)
let iter_k f (x : 'a) =
  let rec k x = f ~k x in
  k x
