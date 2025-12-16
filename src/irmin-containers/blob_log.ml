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

module Blob_log (T : Time.S) (V : Irmin.Type.S) :
  Irmin.Contents.S with type t = (V.t * T.t) list = struct
  type t = (V.t * T.t) list [@@deriving irmin]

  let compare_t = Irmin.Type.(unstage (compare T.t))
  let compare (_, t1) (_, t2) = compare_t t1 t2

  let newer_than timestamp entries =
    let rec util acc = function
      | [] -> List.rev acc
      | (_, x) :: _ when compare_t x timestamp <= 0 -> List.rev acc
      | h :: t -> util (h :: acc) t
    in
    util [] entries

  let merge ~old v1 v2 =
    let open Irmin.Merge.Infix in
    let ok = Irmin.Merge.ok in
    old () >>=* fun old ->
    let old = match old with None -> [] | Some o -> o in
    let l1, l2 =
      match old with
      | [] -> (v1, v2)
      | (_, t) :: _ -> (newer_than t v1, newer_than t v2)
    in
    let l3 = List.sort compare (List.rev_append l1 l2) in
    ok (List.rev_append l3 old)

  let merge = Irmin.Merge.(option (v t merge))
end

module type S = sig
  module Store : Irmin.KV

  type value

  val append : path:Store.path -> Store.t -> value -> unit
  val read_all : path:Store.path -> Store.t -> value list
end

module Make (Backend : Irmin.KV_maker) (T : Time.S) (V : Irmin.Type.S) = struct
  module Store = Backend.Make (Blob_log (T) (V))

  let empty_info = Store.Info.none

  type value = V.t

  let create_entry v = (v, T.now ())

  let append ~path t v =
    match Store.find t path with
    | None -> Store.set_exn ~info:empty_info t path [ create_entry v ]
    | Some l -> Store.set_exn ~info:empty_info t path (create_entry v :: l)

  let read_all ~path t =
    match Store.find t path with
    | None -> []
    | Some l -> List.map (fun (v, _) -> v) l
end

module FS (V : Irmin.Type.S) = Make (Irmin_fs_unix.KV) (Time.Machine) (V)
module Mem (V : Irmin.Type.S) = Make (Irmin_mem.KV) (Time.Machine) (V)
