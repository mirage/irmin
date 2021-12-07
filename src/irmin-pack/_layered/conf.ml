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
open Irmin.Backend.Conf

let spec = Spec.v "layered"

module Key = struct
  let lower_root =
    key ~spec ~doc:"The root directory for the lower layer." "root_lower"
      Irmin.Type.string Default.lower_root

  let upper_root1 =
    key ~spec ~doc:"The root directory for the upper layer." "root_upper"
      Irmin.Type.string Default.upper1_root

  let upper_root0 =
    key ~spec ~doc:"The root directory for the secondary upper layer."
      "root_second" Irmin.Type.string Default.upper0_root

  let with_lower =
    key ~spec ~doc:"Use a lower layer." "with-lower" Irmin.Type.bool
      Default.with_lower

  let blocking_copy_size =
    key ~spec
      ~doc:
        "Specify the maximum size (in bytes) that can be copied in the \
         blocking portion of the freeze."
      "blocking-copy" Irmin.Type.int Default.blocking_copy_size
end

let spec = Spec.join spec [ Pack.spec ]
let lower_root conf = get conf Key.lower_root
let upper_root1 conf = get conf Key.upper_root1
let upper_root0 conf = get conf Key.upper_root0
let with_lower conf = get conf Key.with_lower
let blocking_copy_size conf = get conf Key.blocking_copy_size

let init ?(lower_root = Default.lower_root) ?(upper_root1 = Default.upper1_root)
    ?(upper_root0 = Default.upper0_root) ?(with_lower = Default.with_lower)
    ?(blocking_copy_size = Default.blocking_copy_size) config =
  let with_binding k v c = add c k v in
  let cfg =
    empty spec
    |> with_binding Key.lower_root lower_root
    |> with_binding Key.upper_root1 upper_root1
    |> with_binding Key.upper_root0 upper_root0
    |> with_binding Key.with_lower with_lower
    |> with_binding Key.blocking_copy_size blocking_copy_size
  in
  verify (union config cfg)
