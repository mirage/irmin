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
  let v _config = map

  let clear t =
    Log.debug (fun f -> f "clear");
    t.t <- KMap.empty

  let close _ = Log.debug (fun f -> f "close")
  let cast t = (t :> read_write t)
  let batch t f = f (cast t)
  let pp_key = Irmin.Type.pp K.t

  let find { t; _ } key =
    Log.debug (fun f -> f "find %a" pp_key key);
    try Some (KMap.find key t) with Not_found -> None

  let mem { t; _ } key =
    Log.debug (fun f -> f "mem %a" pp_key key);
    KMap.mem key t
end

module Atomic_write_maker (IO : Irmin.IO.S) = struct
  open IO.Syntax
  module Watch = Irmin.Private.Watch.Make (IO)

  module Make (K : Irmin.Type.S) (V : Irmin.Type.S) = struct
    module RO = Read_only (K) (V)
    module W = Watch.Make (K) (V)
    module L = Irmin.Private.Lock.Make (IO) (K)

    type t = { t : unit RO.t; w : W.t; lock : L.t }
    type key = RO.key
    type value = RO.value
    type watch = W.watch

    let watches = W.v ()
    let lock = L.v ()

    let v config =
      let t = RO.v config in
      Lwt.return { t; w = watches; lock }

    let close t =
      let+ () = W.clear t.w in
      RO.close t.t

    let find t k = IO.return (RO.find t.t k)
    let mem t k = IO.return (RO.mem t.t k)
    let watch_key t = W.watch_key t.w
    let watch t = W.watch t.w
    let unwatch t = W.unwatch t.w

    let list t =
      Log.debug (fun f -> f "list");
      RO.KMap.fold (fun k _ acc -> k :: acc) t.t.RO.t [] |> IO.return

    let set t key value =
      Log.debug (fun f -> f "update");
      let* () =
        L.with_lock t.lock key (fun () ->
            t.t.RO.t <- RO.KMap.add key value t.t.RO.t;
            IO.return ())
      in
      W.notify t.w key (Some value)

    let remove t key =
      Log.debug (fun f -> f "remove");
      let* () =
        L.with_lock t.lock key (fun () ->
            t.t.RO.t <- RO.KMap.remove key t.t.RO.t;
            IO.return ())
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
      let+ () = if updated then W.notify t.w key set else IO.return () in
      updated

    let clear t =
      let+ () = W.clear t.w in
      RO.clear t.t
  end
end

module Direct = struct
  module IO = Irmin.IO.Direct

  module Append_only = struct
    module Make (K : Irmin.Type.S) (V : Irmin.Type.S) = struct
      include Read_only (K) (V)

      let add t key value =
        Log.debug (fun f -> f "add -> %a" pp_key key);
        t.t <- KMap.add key value t.t
    end
  end

  module Content_addressable = Irmin.Content_addressable (IO) (Append_only)
  module Atomic_write = Atomic_write_maker (IO)
end

module IO = Irmin.IO.Lwt
module Merge = Irmin.Merge.Make (IO)
module Append_only = Irmin.Of_direct.Append_only (IO) (Direct.Append_only)

module Content_addressable =
  Irmin.Of_direct.Content_addressable (IO) (Direct.Content_addressable)

module Atomic_write = Atomic_write_maker (IO)

let config () = Irmin.Private.Conf.empty

module Make = struct
  module AO = Irmin.Content_addressable (IO) (Append_only)
  module AW = Atomic_write
  include Irmin.Make (IO) (AO) (AW)
end

module KV = struct
  type 'a io = 'a Lwt.t
  type 'a merge = 'a Merge.t

  module M = Irmin.Metadata.None (Merge)

  module Make (C : Irmin.Contents.S with type 'a merge := 'a Merge.t) =
    Make.Make (M) (C) (Irmin.Path.String_list) (Irmin.Branch.String)
      (Irmin.Hash.BLAKE2B)
end

(* Enforce that {!KV} is a sub-type of {!Irmin.KV_MAKER}. *)
module KV_is_a_KV_MAKER : Irmin.KV_MAKER = KV
