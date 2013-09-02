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

module Graph = IrminKey.Graph(IrminLwt)

exception Empty

let default_front = Tag.of_string "FRONT"
let default_back = Tag.of_string "BACK"

type t = {
  source: source;
  front : Tag.t;
  back  : Tag.t;
  store : IrminLwt.t;
}

let key_store t = key_store t.store
let value_store t = value_store t.store
let tag_store t = tag_store t.store

exception Error of string

let error fmt =
  Printf.kprintf (fun str ->
      Printf.eprintf "fatal: %s\n%!" str;
      raise_lwt (Error str)
    ) fmt

let init t =
  match t.source with
  | Dir f     -> Disk.init f
  | InMemory  -> Lwt.return ()
  | Unix f    ->
    if Sys.file_exists f then Lwt.return ()
    else error "%s does not exist." f

let create ?(front = default_front) ?(back = default_back) source =
  debug "create";
  let store = create ~keys:source ~values:source ~tags:source in
  { source; front; back; store }

let fronts t =
  Tag_store.read (tag_store t) t.front

let backs t =
  Tag_store.read (tag_store t) t.back

let is_empty t =
  lwt fronts = fronts t in
  lwt backs  = backs t in
  Lwt.return (Key.Set.is_empty fronts || Key.Set.is_empty backs)

let graph t =
  lwt fronts = fronts t in
  lwt backs  = backs t in
  if Key.Set.is_empty fronts || Key.Set.is_empty backs then
    Lwt.return (Graph.create ())
  else
    Graph.of_store t.store ~roots:fronts ~sinks:backs ()

let add t value =
  lwt key = Value_store.write (value_store t) value in
  lwt fronts = fronts t in
  lwt backs  = backs t in
  let revision = Value.revision key backs in
  lwt new_back = Value_store.write (value_store t) revision in
  let new_back = Key.Set.singleton new_back in
  lwt () =
    if not (Key.Set.is_empty fronts) then Lwt.return ()
    else Tag_store.update (tag_store t) t.front new_back in
  Tag_store.update (tag_store t) t.back new_back

let empty fmt =
  Printf.kprintf (fun str ->
      IrminMisc.error "QUEUE" "%s" str;
      raise_lwt Empty
    ) fmt

let value_of_key t key =
  lwt v = Value_store.read (value_store t) key in
  match v with
  | None   -> empty "No value!"
  | Some v -> Lwt.return v

let contents t key =
  lwt v = value_of_key t key in
  match Value.contents v with
  | None   -> empty "No content!"
  | Some k -> value_of_key t k

let to_list t =
  lwt ga = Graph.of_store t.store () in
  lwt () = Graph.dump t.store ga "ALL" in
  lwt g = graph t in
  lwt () = Graph.dump t.store g "FRONTS" in
  let keys = Graph.Topological.fold (fun key acc -> key :: acc) g [] in
  lwt values = Lwt_list.fold_left_s (fun acc key ->
      lwt value = contents t key in
      Lwt.return (value :: acc)
    ) [] keys in
  Lwt.return values

let peek t =
  lwt fronts = fronts t in
  try
    let key = Key.Set.choose fronts in
    contents t key
  with Not_found ->
    empty "FRONT"

let take t =
  lwt fronts = Tag_store.read (tag_store t) t.front in
  lwt key =
    try Lwt.return (Key.Set.choose fronts)
    with Not_found -> empty "FRONT" in
  let fronts = Key.Set.remove key fronts in
  let move_front new_fronts =
    let new_fronts = Key.Set.union new_fronts fronts in
    lwt () =
      if Key.Set.is_empty new_fronts then Tag_store.remove (tag_store t) t.front
      else Tag_store.update (tag_store t) t.front new_fronts in
    contents t key in
  (* XXX: use a staging area *)
  lwt g = graph t in
  match Graph.succ g key with
  | []   -> move_front Key.Set.empty
  | keys ->
    lwt new_fronts = Lwt_list.filter_s (fun key ->
        lwt preds = Key_store.pred (key_store t) key in
        let todo = Key.Set.inter preds fronts in
        Lwt.return (Key.Set.is_empty todo)
      ) keys in
    move_front (Key.Set.of_list new_fronts)

let server t ~limit file =
  let fd = IrminIO.Lwt_channel.unix_socket_server ~limit file in
  match t.source with
  | Dir d    ->
    debug "on-disk server";
    DiskServer.run ~timeout:2. (Disk.create d) fd
  | Unix _   -> failwith "TODO"
  | InMemory ->
    debug "in-memory server";
    MemoryServer.run ~timeout:2. (Memory.create ()) fd


let watch _ =
  failwith "TODO"

let pull _ =
  failwith "TODO"

let push _ =
  failwith "TODO"

let clone _ =
  failwith "TODO"
