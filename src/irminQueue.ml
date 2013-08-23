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
  | `Dir f -> Lwt_unix.run (Disk.init f)

let create = function
  | `Dir f -> Disk.create f

let add f = function
  | []     -> ()
  | values ->
    let result =
      let t = create f in
      lwt keys = Lwt_list.map_s (fun value ->
          lwt key = Disk.Value_store.write t value in
          Lwt.return (key, value)
        ) values in
      List.iter (fun (key, value) ->
          debug "add: %s %s\n" (Key.pretty key) (Value.pretty value)
        ) keys;
      let key_head = fst (List.hd (List.rev keys)) in
      debug "key-head: %s" (Key.pretty key_head);
      lwt () = Disk.Tag_store.update t head key_head in

      Disk.Value_store.dump t
    in
    Lwt_unix.run result

let watch _ =
  failwith "TODO"

let peek f =
  let result =
    let t = create f in
    lwt head = Disk.Tag_store.read t head in
    match head with
    | None   -> raise Empty
    | Some h ->
      lwt keys = Disk.Key_store.pred t h in
      match Key.Set.to_list keys with
      | []   -> raise Empty
      | k::_ ->
        lwt v = Disk.Value_store.read t k in
        match v with
        | None   -> raise Empty
        | Some v -> Lwt.return v in
  Lwt_unix.run result

let take _ =
  failwith "TODO"

let dump _ =
  failwith "TODO"

let pull _ =
  failwith "TODO"

let push _ =
  failwith "TODO"

let clone _ =
  failwith "TODO"
