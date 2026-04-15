(*
 * Copyright (c) 2020 KC Sivaramakrishnan <kc@kcsrk.info>
 * Copyright (c) 2020 Anirudh Sunder Raj <anirudh6626@gmail.com>
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

open! Import

module Counter : Irmin.Contents.S with type t = int64 = struct
  type t = int64

  let t = Irmin.Type.int64
  let merge = Irmin.Merge.(option counter)
end

module type S = sig
  module Store : Irmin.KV

  val inc :
    ?by:int64 -> ?info:Store.Info.f -> path:Store.path -> Store.t -> unit

  val dec :
    ?by:int64 -> ?info:Store.Info.f -> path:Store.path -> Store.t -> unit

  val read : path:Store.path -> Store.t -> int64
end

module Make (Backend : Irmin.KV_maker) = struct
  module Store = Backend.Make (Counter)

  let empty_info = Store.Info.none

  let modify by info t path fn =
    match Store.find t path with
    | None -> Store.set_exn ~info t path (fn 0L by)
    | Some v -> Store.set_exn ~info t path (fn v by)

  let inc ?(by = 1L) ?(info = empty_info) ~path t =
    modify by info t path (fun x by -> Int64.add x by)

  let dec ?(by = 1L) ?(info = empty_info) ~path t =
    modify by info t path (fun x by -> Int64.sub x by)

  let read ~path t = Store.find t path |> function None -> 0L | Some v -> v
end

module FS = Make (Irmin_fs_unix.KV)
module Mem = Make (Irmin_mem.KV)
