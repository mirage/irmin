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

module Default = struct
  let lower_root = Irmin_layers.Layer_id.to_string `Lower
  let upper0_root = Irmin_layers.Layer_id.to_string `Upper0
  let upper1_root = Irmin_layers.Layer_id.to_string `Upper1
  let with_lower = true
  let blocking_copy_size = 64
end

module Pack = Irmin_pack.Conf

module Conf = Irmin.Private.Conf.Extend (struct
  include Pack
end)

include Conf

let lower_root_key =
  Conf.key ~doc:"The root directory for the lower layer." "root_lower"
    Irmin.Type.string Default.lower_root

let lower_root conf = Conf.get conf lower_root_key

let upper_root1_key =
  Conf.key ~doc:"The root directory for the upper layer." "root_upper"
    Irmin.Type.string Default.upper1_root

let upper_root1 conf = Conf.get conf upper_root1_key

let upper_root0_key =
  Conf.key ~doc:"The root directory for the secondary upper layer."
    "root_second" Irmin.Type.string Default.upper0_root

let upper_root0 conf = Conf.get conf upper_root0_key

let with_lower_key =
  Conf.key ~doc:"Use a lower layer." "with-lower" Irmin.Type.bool
    Default.with_lower

let with_lower conf = Conf.get conf with_lower_key

let blocking_copy_size_key =
  Conf.key
    ~doc:
      "Specify the maximum size (in bytes) that can be copied in the blocking \
       portion of the freeze."
    "blocking-copy" Irmin.Type.int Default.blocking_copy_size

let blocking_copy_size conf = Conf.get conf blocking_copy_size_key

let v ?(lower_root = Default.lower_root) ?(upper_root1 = Default.upper1_root)
    ?(upper_root0 = Default.upper0_root) ?(with_lower = Default.with_lower)
    ?(blocking_copy_size = Default.blocking_copy_size) config =
  let with_binding k v c = Conf.add c k v in
  config
  |> with_binding lower_root_key lower_root
  |> with_binding upper_root1_key upper_root1
  |> with_binding upper_root0_key upper_root0
  |> with_binding with_lower_key with_lower
  |> with_binding blocking_copy_size_key blocking_copy_size
