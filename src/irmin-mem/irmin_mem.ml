(*
 * Copyright (c) 2013-2017 Thomas Gazagnaire <thomas@gazagnaire.org>
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

open Lwt.Infix

let src = Logs.Src.create "irmin.mem" ~doc:"Irmin in-memory store"
module Log = (val Logs.src_log src : Logs.LOG)

module Read_only (K: Irmin.Type.S) (V: Irmin.Type.S) = struct

  module KMap = Map.Make(struct
      type t = K.t
      let compare = Irmin.Type.compare K.t
    end)

  type key = K.t
  type value = V.t
  type 'a t = { mutable t: value KMap.t }
  let map = { t = KMap.empty }
  let v _config = Lwt.return map

  let cast t = (t :> [`Read | `Write] t)
  let batch t f = f (cast t)

  let pp_key = Irmin.Type.pp K.t

  let find { t; _ } key =
    Log.debug (fun f -> f "find %a" pp_key key);
    try Lwt.return (Some (KMap.find key t))
    with Not_found -> Lwt.return_none

  let mem { t; _ } key =
    Log.debug (fun f -> f "mem %a" pp_key key);
    Lwt.return (KMap.mem key t)

end

module Append_only (K: Irmin.Type.S) (V: Irmin.Type.S) = struct

  include Read_only(K)(V)

  let add t key value =
    Log.debug (fun f -> f "add -> %a" pp_key key);
    t.t <- KMap.add key value t.t;
    Lwt.return ()

end

module Atomic_write (K: Irmin.Type.S) (V: Irmin.Type.S) = struct

  module RO = Read_only(K)(V)
  module W = Irmin.Private.Watch.Make(K)(V)
  module L = Irmin.Private.Lock.Make(K)

  type t = { t: unit RO.t; w: W.t; lock: L.t }
  type key = RO.key
  type value = RO.value
  type watch = W.watch

  let watches = W.v ()
  let lock = L.v ()

  let v config =
    RO.v config >>= fun t ->
    Lwt.return { t; w = watches; lock }

  let find t = RO.find t.t
  let mem t = RO.mem t.t
  let watch_key t = W.watch_key t.w
  let watch t = W.watch t.w
  let unwatch t = W.unwatch t.w

  let list t =
    Log.debug (fun f -> f "list");
    RO.KMap.fold (fun k _ acc -> k :: acc) t.t.RO.t []
    |> Lwt.return

  let set t key value =
    Log.debug (fun f -> f "update");
    L.with_lock t.lock key (fun () ->
        t.t.RO.t <- RO.KMap.add key value t.t.RO.t;
        Lwt.return_unit
      ) >>= fun () ->
    W.notify t.w key (Some value)

  let remove t key =
    Log.debug (fun f -> f "remove");
    L.with_lock t.lock key (fun () ->
        t.t.RO.t <- RO.KMap.remove key t.t.RO.t ;
        Lwt.return_unit
      ) >>= fun () ->
    W.notify t.w key None

  let test_and_set t key ~test ~set =
    Log.debug (fun f -> f "test_and_set");
    L.with_lock t.lock key (fun () ->
        find t key >>= fun v ->
        if Irmin.Type.(equal (option V.t)) test v then (
          let () = match set with
            | None   -> t.t.RO.t <- RO.KMap.remove key t.t.RO.t
            | Some v -> t.t.RO.t <- RO.KMap.add key v t.t.RO.t
          in
          Lwt.return true
        ) else
          Lwt.return false
      ) >>= fun updated ->
    (if updated then W.notify t.w key set else Lwt.return_unit) >>= fun () ->
    Lwt.return updated

end

let config () = Irmin.Private.Conf.empty

module Make = Irmin.Make(Irmin.Content_addressable(Append_only))(Atomic_write)

module KV (C: Irmin.Contents.S) =
  Make
    (Irmin.Metadata.None)
    (C)
    (Irmin.Path.String_list)
    (Irmin.Branch.String)
    (Irmin.Hash.SHA1)
