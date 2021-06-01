(*
 * Copyright (c) 2013-2021 Thomas Gazagnaire <thomas@gazagnaire.org>
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

let src = Logs.Src.create "irmin.mem" ~doc:"Irmin in-memory store"

module Log = (val Logs.src_log src : Logs.LOG)

module Read_only (K : Irmin.Type.S) (V : Irmin.Type.S) = struct
  module KMap = Map.Make (struct
    type t = K.t

    let compare = Irmin.Type.(unstage (compare K.t))
  end)

  type key = K.t
  type value = V.t
  type 'a t = { mutable t : value KMap.t }

  let map = { t = KMap.empty }
  let v _config = Lwt.return map

  let clear t =
    Log.debug (fun f -> f "clear");
    t.t <- KMap.empty;
    Lwt.return_unit

  let close _ =
    Log.debug (fun f -> f "close");
    Lwt.return_unit

  let cast t = (t :> read_write t)
  let batch t f = f (cast t)
  let pp_key = Irmin.Type.pp K.t

  let find { t; _ } key =
    Log.debug (fun f -> f "find %a" pp_key key);
    try Lwt.return_some (KMap.find key t) with Not_found -> Lwt.return_none

  let mem { t; _ } key =
    Log.debug (fun f -> f "mem %a" pp_key key);
    Lwt.return (KMap.mem key t)
end

module Append_only (K : Irmin.Type.S) (V : Irmin.Type.S) = struct
  include Read_only (K) (V)

  let add t key value =
    Log.debug (fun f -> f "add -> %a" pp_key key);
    t.t <- KMap.add key value t.t;
    Lwt.return_unit
end

module Atomic_write (K : Irmin.Type.S) (V : Irmin.Type.S) = struct
  module RO = Read_only (K) (V)
  module W = Irmin.Private.Watch.Make (K) (V)
  module L = Irmin.Private.Lock.Make (K)

  type t = { t : unit RO.t; w : W.t; lock : L.t }
  type key = RO.key
  type value = RO.value
  type watch = W.watch

  let watches = W.v ()
  let lock = L.v ()

  let v config =
    let* t = RO.v config in
    Lwt.return { t; w = watches; lock }

  let close t = W.clear t.w >>= fun () -> RO.close t.t
  let find t = RO.find t.t
  let mem t = RO.mem t.t
  let watch_key t = W.watch_key t.w
  let watch t = W.watch t.w
  let unwatch t = W.unwatch t.w

  let list t =
    Log.debug (fun f -> f "list");
    RO.KMap.fold (fun k _ acc -> k :: acc) t.t.RO.t [] |> Lwt.return

  let set t key value =
    Log.debug (fun f -> f "update");
    let* () =
      L.with_lock t.lock key (fun () ->
          t.t.RO.t <- RO.KMap.add key value t.t.RO.t;
          Lwt.return_unit)
    in
    W.notify t.w key (Some value)

  let remove t key =
    Log.debug (fun f -> f "remove");
    let* () =
      L.with_lock t.lock key (fun () ->
          t.t.RO.t <- RO.KMap.remove key t.t.RO.t;
          Lwt.return_unit)
    in
    W.notify t.w key None

  let equal_v_opt = Irmin.Type.(unstage (equal (option V.t)))

  let test_and_set t key ~test ~set =
    Log.debug (fun f -> f "test_and_set");
    let* updated =
      L.with_lock t.lock key (fun () ->
          let+ v = find t key in
          if equal_v_opt test v then
            let () =
              match set with
              | None -> t.t.RO.t <- RO.KMap.remove key t.t.RO.t
              | Some v -> t.t.RO.t <- RO.KMap.add key v t.t.RO.t
            in
            true
          else false)
    in
    let+ () = if updated then W.notify t.w key set else Lwt.return_unit in
    updated

  let clear t = W.clear t.w >>= fun () -> RO.clear t.t
end

let config () = Irmin.Private.Conf.empty

module Content_addressable = Irmin.Content_addressable.Make (Append_only)
module S = Irmin.Maker (Content_addressable) (Atomic_write)
module KV = Irmin.KV_maker (Content_addressable) (Atomic_write)
include S

(* Enforce that {!S} is a sub-type of {!Irmin.Maker}. *)
module _ : Irmin.Maker = S

(* Enforce that {!KV} is a sub-type of {!Irmin.KV_maker}. *)
module _ : Irmin.KV_maker = KV
