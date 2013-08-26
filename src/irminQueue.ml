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

let init = function
  | `Dir f -> Disk.init f

let create = function
  | `Dir f -> Disk.create f

let is_empty f =
  debug "is-empty";
  let t = create f in
  lwt tag_head = Disk.Tag_store.read t head in
  lwt tag_tail = Disk.Tag_store.read t tail in
  match tag_head, tag_tail with
  | None  , None   -> Lwt.return true
  | Some _, Some _ -> Lwt.return false
  | _ -> failwith "is_empty"

let add f = function
  | []     -> Lwt.return ()
  | values ->
    debug "add";
    lwt empty = is_empty f in
    let t = create f in
    lwt preds = Lwt_list.fold_left_s (fun preds value ->
        lwt key = Disk.Value_store.write t value in
        debug "add blob key:%s value:%s" (Key.pretty key) (Value.pretty value);
        let revision = Value.revision key preds in
        lwt key_head = Disk.Value_store.write t revision in
        debug "add revision key:%s value:%s"
          (Key.pretty key_head) (Value.pretty revision);
        Lwt.return [key_head]
      ) [] values in
    let key_head = match preds with
      | []   -> failwith "IrminQueue.add"
      | k::_ -> k in
    lwt () = Disk.Tag_store.update t head key_head in
    lwt () =
      if empty then Disk.Tag_store.update t tail key_head
      else Lwt.return () in
    Disk.Value_store.dump t

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

let peek f =
  let t = create f in
  lwt head = Disk.Tag_store.read t head in
  match head with
  | None   -> empty "No head!"
  | Some h ->
    lwt keys = Disk.Key_store.pred t h in
    match Key.Set.to_list keys with
    | []   -> empty "No pred!"
    | k::_ ->
      lwt v = value_of_key t k in
      match Value.contents v with
      | None  -> empty "No content!"
      | Some k -> value_of_key t k

let to_list _ =
  failwith "TODO"

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
