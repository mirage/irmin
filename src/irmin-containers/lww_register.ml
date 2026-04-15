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

module LWW (T : Time.S) (V : Irmin.Type.S) :
  Irmin.Contents.S with type t = V.t * T.t = struct
  type t = V.t * T.t [@@deriving irmin]

  let compare_t = Irmin.Type.(unstage (compare T.t))
  let compare_v = Irmin.Type.(unstage (compare V.t))

  let compare (v1, t1) (v2, t2) =
    let res = compare_t t1 t2 in
    if res = 0 then compare_v v1 v2 else res

  let merge ~old:_ v1 v2 =
    let open Irmin.Merge in
    if compare v1 v2 > 0 then ok v1 else ok v2

  let merge = Irmin.Merge.(option (v t merge))
end

module type S = sig
  module Store : Irmin.KV

  type value

  val read : path:Store.path -> Store.t -> value option
  val write : ?info:Store.Info.f -> path:Store.path -> Store.t -> value -> unit
end

module Make (Backend : Irmin.KV_maker) (T : Time.S) (V : Irmin.Type.S) = struct
  module Store = Backend.Make (LWW (T) (V))

  let empty_info = Store.Info.none

  type value = V.t

  let read ~path t =
    Store.find t path |> function None -> None | Some (v, _) -> Some v

  let write ?(info = empty_info) ~path t v =
    let timestamp = T.now () in
    Store.set_exn ~info t path (v, timestamp)
end

module FS (V : Irmin.Type.S) = Make (Irmin_fs_unix.KV) (Time.Machine) (V)
module Mem (V : Irmin.Type.S) = Make (Irmin_mem.KV) (Time.Machine) (V)
