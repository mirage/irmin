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

let return = Lwt.return
let empty_info = Irmin.Info.none

module Counter : Irmin.Contents.S with type t = int64 = struct
  type t = int64

  let t = Irmin.Type.int64
  let merge = Irmin.Merge.(option counter)
end

module type S = sig
  module Store : Irmin.S

  val inc :
    ?by:int64 -> ?info:Irmin.Info.f -> path:Store.key -> Store.t -> unit Lwt.t

  val dec :
    ?by:int64 -> ?info:Irmin.Info.f -> path:Store.key -> Store.t -> unit Lwt.t

  val read : path:Store.key -> Store.t -> int64 Lwt.t
end

module Make (Backend : Stores.Store_maker) = struct
  module Store = Backend (Counter)

  let modify by info t path fn =
    Store.find t path >>= function
    | None -> Store.set_exn ~info t path (fn 0L by)
    | Some v -> Store.set_exn ~info t path (fn v by)

  let inc ?(by = 1L) ?(info = empty_info) ~path t =
    modify by info t path (fun x by -> Int64.add x by)

  let dec ?(by = 1L) ?(info = empty_info) ~path t =
    modify by info t path (fun x by -> Int64.sub x by)

  let read ~path t =
    Store.find t path >>= function None -> return 0L | Some v -> return v
end

module FS = Make (Irmin_unix.FS.KV)
module Mem = Make (Irmin_mem.KV)
