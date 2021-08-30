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

module Append_only (Hash : Irmin.Hash.S) (Value : Irmin.Type.S) = struct
  module Hashes = Map.Make (struct
    type t = Hash.t [@@deriving irmin ~compare]
  end)

  module Key = Irmin.Key.Of_hash (Hash)

  type key = Key.t [@@deriving irmin ~pp]
  type value = Value.t
  type hash = Hash.t [@@deriving irmin ~pp]
  type 'a t = { mutable t : value Hashes.t }

  let map = { t = Hashes.empty }
  let v _config = Lwt.return map

  let clear t =
    Log.debug (fun f -> f "clear");
    (* Hashes.iter (fun _ (k, _) -> Key.clear k) t.t; *)
    t.t <- Hashes.empty;
    Lwt.return_unit

  let close _ =
    Log.debug (fun f -> f "close");
    Lwt.return_unit

  let cast t = (t :> read_write t)
  let batch t f = f (cast t)

  let find_aux { t; _ } key =
    try Some (Hashes.find (Key.to_hash key) t) with Not_found -> None

  let index _ h =
    Log.debug (fun f -> f "index %a" pp_hash h);
    Lwt.return (Some (Key.of_hash h))

  let find t key =
    Log.debug (fun f -> f "find %a" pp_key key);
    let v = find_aux t key in
    Lwt.return v

  let mem t key =
    Log.debug (fun f -> f "mem %a" pp_key key);
    match find_aux t key with
    | None -> Lwt.return false
    | Some _ -> Lwt.return true

  let add t hash value =
    Log.debug (fun f -> f "add -> %a" pp_hash hash);
    t.t <- Hashes.add hash value t.t;
    Lwt.return (Key.of_hash hash)
end

module Atomic_write (K : Irmin.Type.S) (V : Irmin.Type.S) = struct
  module Key = K
  module W = Irmin.Private.Watch.Make (K) (V)
  module L = Irmin.Private.Lock.Make (K)

  module Keys = Map.Make (struct
    type t = K.t [@@deriving irmin ~compare]
  end)

  type value = V.t
  type key = Key.t
  type watch = W.watch

  let watches = W.v ()
  let lock = L.v ()
  let pp_key = Irmin.Type.pp K.t

  type t = { mutable t : value Keys.t; w : W.t; lock : L.t }

  let v =
    let t = { t = Keys.empty; w = watches; lock } in
    fun _config -> Lwt.return t

  let close t = W.clear t.w

  let find t k =
    Log.debug (fun f -> f "find %a" pp_key k);
    Lwt.return (Keys.find_opt k t.t)

  let mem t k =
    Log.debug (fun f -> f "mem %a" pp_key k);
    Lwt.return (Keys.mem k t.t)

  let watch_key t = W.watch_key t.w
  let watch t = W.watch t.w
  let unwatch t = W.unwatch t.w

  let list t =
    Log.debug (fun f -> f "list");
    Keys.fold (fun k _ acc -> k :: acc) t.t [] |> Lwt.return

  let set t key value =
    Log.debug (fun f -> f "update");
    let* () =
      L.with_lock t.lock key (fun () ->
          t.t <- Keys.add key value t.t;
          Lwt.return_unit)
    in
    W.notify t.w key (Some value)

  let remove t key =
    Log.debug (fun f -> f "remove");
    let* () =
      L.with_lock t.lock key (fun () ->
          t.t <- Keys.remove key t.t;
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
              | None -> t.t <- Keys.remove key t.t
              | Some v -> t.t <- Keys.add key v t.t
            in
            true
          else false)
    in
    let+ () = if updated then W.notify t.w key set else Lwt.return_unit in
    updated

  let clear t =
    t.t <- Keys.empty;
    W.clear t.w
end

let config () = Irmin.Private.Conf.empty

module Content_addressable = Irmin.Content_addressable.Make (Append_only)
module S = Irmin.Maker (Content_addressable) (Atomic_write)
module KV = Irmin.KV_maker (Content_addressable) (Atomic_write)
include S

(* Enforce that {!S} is a sub-type of {!Irmin.Maker}. *)
module Maker_is_a_maker : Irmin.Maker = S

(* Enforce that {!KV} is a sub-type of {!Irmin.KV_maker}. *)
module KV_is_a_KV : Irmin.KV_maker = KV
