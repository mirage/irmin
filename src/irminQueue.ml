(*
 * Copyright (c) 2013 Thomas Gazagnaire <thomas@gazagnaire.org>
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

open IrminTypes
open IrminLwt

let debug fmt = IrminMisc.debug "QUEUE" fmt

let head = Tag.of_name "HEAD"
let tail = Tag.of_name "TAIL"

exception Empty

type t =
  [ `Dir of string ]

type state = {
  disk: Disk.t;
  head: Key.t option;
  tail: Key.t option;
}

let init = function
  | `Dir f -> Disk.init f

let create = function
  | `Dir f ->
    let disk = Disk.create f in
    lwt head = Disk.Tag_store.read disk head in
    lwt tail = Disk.Tag_store.read disk tail in
    Lwt.return { disk; head; tail }

let is_empty t =
  debug "is-empty";
  match t.head, t.tail with
  | None  , None   -> Lwt.return true
  | Some _, Some _ -> Lwt.return false
  | _ -> failwith "is_empty"

let add f = function
  | []     -> Lwt.return ()
  | values ->
    debug "add";
    lwt t = create f in
    lwt empty = is_empty t in
    let preds = match t.head with
      | None   -> []
      | Some h -> [h] in
    lwt preds = Lwt_list.fold_left_s (fun preds value ->
        lwt key = Disk.Value_store.write t.disk value in
        debug "add blob key:%s value:%s" (Key.pretty key) (Value.pretty value);
        let revision = Value.revision key preds in
        lwt key_head = Disk.Value_store.write t.disk revision in
        debug "add revision key:%s value:%s"
          (Key.pretty key_head) (Value.pretty revision);
        Lwt.return [key_head]
      ) preds values in
    let key_head = match preds with
      | []   -> failwith "IrminQueue.add"
      | k::_ -> k in
    lwt () = Disk.Tag_store.update t.disk head key_head in
    lwt () =
      if empty then Disk.Tag_store.update t.disk tail key_head
      else Lwt.return () in
    Disk.Value_store.dump t.disk

let empty fmt =
  Printf.kprintf (fun str ->
      IrminMisc.error "QUEUE" "%s" str;
      raise_lwt Empty
    ) fmt

let value_of_key t key =
  lwt v = Disk.Value_store.read t key in
  match v with
  | None   -> empty "No value!"
  | Some v -> Lwt.return v

let contents t key =
  lwt v = value_of_key t.disk key in
  match Value.contents v with
  | None   -> empty "No content!"
  | Some k -> value_of_key t.disk k

let peek f =
  lwt t = create f in
  match t.head with
  | None   -> empty "No head!"
  | Some h ->
    lwt keys = Disk.Key_store.pred t.disk h in
    match Key.Set.to_list keys with
    | []   -> empty "No pred!"
    | k::_ -> contents t k

module Graph = IrminKey.Graph(Disk.Key_store)(Disk.Value_store)(Disk.Tag_store)

let to_list f =
  lwt t = create f in
  match t.head, t.tail with
  | None, _  | _, None   -> Lwt.return []
  | Some head, Some tail ->
    lwt g = Graph.of_store t.disk [head] in
    lwt ga = Graph.of_store t.disk [] in
    lwt () = Graph.dump t.disk g "HEAD" in
    lwt () = Graph.dump t.disk ga "ALL" in
    let keys = Graph.Topological.fold (fun key acc ->
        let preds = Key.Set.of_list (Graph.pred g key) in
        debug "PREDS(%s) = %s" (Key.pretty key) (Key.Set.pretty preds);
        if key = tail || Key.Set.mem tail preds then key :: acc
        else acc
      ) g [] in
    lwt values = Lwt_list.fold_left_s (fun acc key ->
        lwt value = contents t key in
        Lwt.return (value :: acc)
      ) [] keys in
    Lwt.return values

let take _ =
  failwith "TODO"

let watch _ =
  failwith "TODO"

let dump _ =
  failwith "TODO"

let pull _ =
  failwith "TODO"

let push _ =
  failwith "TODO"

let clone _ =
  failwith "TODO"

let is_empty f =
  lwt t = create f in
  is_empty t
