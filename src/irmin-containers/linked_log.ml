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

module Log_item (T : Time.S) (H : Irmin.Hash.S) (V : Irmin.Type.S) = struct
  type t = { time : T.t; msg : V.t; prev : H.t option } [@@deriving irmin]
end

module Store_item (T : Time.S) (H : Irmin.Hash.S) (V : Irmin.Type.S) = struct
  module L = Log_item (T) (H) (V)

  type t = Value of L.t | Merge of L.t list [@@deriving irmin]
end

module Linked_log
    (C : Stores.Content_addressable)
    (T : Time.S)
    (H : Irmin.Hash.S)
    (V : Irmin.Type.S) =
struct
  type t = H.t [@@deriving irmin]

  module L = Log_item (T) (H) (V)
  module S = Store_item (T) (H) (V)

  module Store = struct
    module CAS = C.Make (H) (Store_item (T) (H) (V))

    let store = ref None

    (* TODO: Fix this hellhole *)
    let get_store () =
      match !store with
      | None ->
          Eio.Switch.run @@ fun sw ->
          let st = CAS.v ~sw @@ C.config in
          store := Some st;
          st
      | Some st -> st

    let read st k = CAS.find st k

    let read_exn st k =
      match CAS.find st k with
      | None -> failwith "key not found in the store"
      | Some v -> v

    let add st v = CAS.batch st (fun t -> CAS.add t v)
  end

  let append prev msg =
    let store = Store.get_store () in
    Store.add store (Value { time = T.now (); msg; prev })

  let read_key k =
    let store = Store.get_store () in
    Store.read_exn store k

  let compare_t = Irmin.Type.(unstage (compare T.t))
  let sort l = List.sort (fun i1 i2 -> compare_t i2.L.time i1.L.time) l

  let merge ~old:_ v1 v2 =
    let open Irmin.Merge in
    let store = Store.get_store () in
    let v1 = Store.read store v1 in
    let v2 = Store.read store v2 in
    let convert_to_list = function
      | None -> []
      | Some (S.Value v) -> [ v ]
      | Some (S.Merge lv) -> lv
    in
    let lv1 = convert_to_list v1 in
    let lv2 = convert_to_list v2 in
    Store.add store (S.Merge (sort @@ lv1 @ lv2)) |> ok

  let merge = Irmin.Merge.(option (v t merge))
end

module type S = sig
  include Blob_log.S

  type cursor

  val get_cursor : path:Store.path -> Store.t -> cursor
  val read : num_items:int -> cursor -> value list * cursor
end

module Make
    (Backend : Irmin.KV_maker)
    (C : Stores.Content_addressable)
    (T : Time.S)
    (H : Irmin.Hash.S)
    (V : Irmin.Type.S)
    () =
struct
  module L = Linked_log (C) (T) (H) (V)
  module Store = Backend.Make (L)

  module Set_elt = struct
    type t = H.t

    let compare = Irmin.Type.(unstage (compare H.t))
  end

  module HashSet = Set.Make (Set_elt)

  type value = V.t

  type cursor = {
    seen : HashSet.t;
    cache : Log_item(T)(H)(V).t list;
    store : Store.t;
  }

  let empty_info = Store.Info.none

  let append ~path t e =
    let prev = Store.find t path in
    let v = L.append prev e in
    Store.set_exn ~info:empty_info t path v

  let get_cursor ~path store =
    let mk_cursor seen cache = { seen; cache; store } in
    match Store.find store path with
    | None -> mk_cursor HashSet.empty []
    | Some k -> (
        match L.read_key k with
        | Value v -> mk_cursor (HashSet.singleton k) [ v ]
        | Merge l -> mk_cursor (HashSet.singleton k) l)

  let rec read_log cursor num_items acc =
    if num_items <= 0 then (List.rev acc, cursor)
    else
      match cursor.cache with
      | [] -> (List.rev acc, cursor)
      | { msg; prev = None; _ } :: xs ->
          read_log { cursor with cache = xs } (num_items - 1) (msg :: acc)
      | { msg; prev = Some pk; _ } :: xs -> (
          if HashSet.mem pk cursor.seen then
            read_log { cursor with cache = xs } (num_items - 1) (msg :: acc)
          else
            let seen = HashSet.add pk cursor.seen in
            match L.read_key pk with
            | Value v ->
                read_log
                  { cursor with seen; cache = L.sort (v :: xs) }
                  (num_items - 1) (msg :: acc)
            | Merge l ->
                read_log
                  { cursor with seen; cache = L.sort (l @ xs) }
                  (num_items - 1) (msg :: acc))

  let read ~num_items cursor = read_log cursor num_items []
  let read_all ~path t = get_cursor t ~path |> read ~num_items:max_int |> fst
end

module FS (C : Stores.Content_addressable) (V : Irmin.Type.S) () =
  Make (Irmin_fs_unix.KV) (C) (Time.Machine) (Irmin.Hash.SHA1) (V) ()

module Mem (C : Stores.Content_addressable) (V : Irmin.Type.S) () =
  Make (Irmin_mem.KV) (C) (Time.Machine) (Irmin.Hash.SHA1) (V) ()
