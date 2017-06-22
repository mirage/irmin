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

module RO (K: Irmin.Contents.Conv) (V: Irmin.Contents.Conv) = struct

  module KMap = Map.Make(struct
      type t = K.t
      let compare = Irmin.Type.compare K.t
    end)

  type key = K.t
  type value = V.t
  type t = { mutable t: value KMap.t }
  let map = { t = KMap.empty }
  let v _config = Lwt.return map

  let find { t; _ } key =
    Log.debug (fun f -> f "find %a" K.pp key);
    try Lwt.return (Some (KMap.find key t))
    with Not_found -> Lwt.return_none

  let mem { t; _ } key =
    Log.debug (fun f -> f "mem %a" K.pp key);
    Lwt.return (KMap.mem key t)

end

module AO (K: Irmin.Hash.S) (V: Irmin.Contents.Conv) = struct

  include RO(K)(V)

  let add t value =
    let key = K.digest V.t value in
    Log.debug (fun f -> f "add -> %a" K.pp key);
    t.t <- KMap.add key value t.t;
    Lwt.return key

end

module Link (K: Irmin.Hash.S) = struct

  include RO(K)(K)

  let add t index key =
    Log.debug (fun f -> f "add link");
    t.t <- KMap.add index key t.t;
    Lwt.return_unit

end

module RW (K: Irmin.Contents.Conv) (V: Irmin.Contents.Conv) = struct

  module RO = RO(K)(V)
  module W = Irmin.Private.Watch.Make(K)(V)
  module L = Irmin.Private.Lock.Make(K)

  type t = { t: RO.t; w: W.t; lock: L.t }
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

module Make = Irmin.Make(AO)(RW)

module KV (C: Irmin.Contents.S) =
  Make
    (Irmin.Metadata.None)
    (C)
    (Irmin.Path.String_list)
    (Irmin.Branch.String)
    (Irmin.Hash.SHA1)
