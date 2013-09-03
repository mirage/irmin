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

open IrminLwt

let debug fmt = IrminMisc.debug "QUEUE" fmt

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


let empty fmt =
  Printf.kprintf (fun str ->
      IrminMisc.error "QUEUE" "%s" str;
      raise_lwt Empty
    ) fmt

let read_key t key =
  lwt value = Value_store.read (value_store t) key in
  match value with
  | None   -> empty "No value!"
  | Some v -> Lwt.return v

let read_contents t key =
  lwt value = read_key t key in
  match Value.contents value with
  | None   -> empty "No content!"
  | Some k -> read_key t k

let dump t name =
  lwt tags = Tag_store.all (tag_store t) in
  lwt labels = Lwt_list.fold_left_s (fun tags tag ->
      lwt keys = Tag_store.read (tag_store t) tag in
      let labels =
        Key.Set.fold (fun key tags -> (key, Tag.to_string tag) :: tags) keys tags in
      Lwt.return labels
    ) [] (Tag.Set.to_list tags) in
  lwt g = Key_store.keys (key_store t) () in
  let keys = Key.Set.to_list (Key.Graph.vertex g) in
  lwt labels = Lwt_list.fold_left_s (fun labels key ->
      lwt value = read_key t key in
      let labels =
        if Value.is_blob value then (key, "blob:"^Value.pretty value) :: labels
        else labels in
      Lwt.return labels
    ) labels keys in
  lwt overlay = Lwt_list.fold_left_s (fun overlay key ->
      lwt value = read_key t key in
      let overlay = match Value.contents value with
        | None  -> overlay
        | Some k -> ((k, key) :: overlay) in
      Lwt.return overlay
    ) [] keys in
  Key.Graph.dump g ~labels ~overlay name;
  Lwt.return ()

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

let add t value =
  lwt () = dump t "before-add" in
  lwt key = Value_store.write (value_store t) value in
  lwt () = Key_store.add (key_store t) key Key.Set.empty in
  lwt fronts = fronts t in
  lwt backs  = backs t in
  let revision_value = Value.revision (Some key) backs in
  lwt revision_key = Value_store.write (value_store t) revision_value in
  lwt () = Key_store.add (key_store t) revision_key backs in
  let new_back = Key.Set.singleton revision_key in
  lwt () =
    if not (Key.Set.is_empty fronts) then Lwt.return ()
    else Tag_store.update (tag_store t) t.front new_back in
  lwt () = Tag_store.update (tag_store t) t.back new_back in
  lwt () = dump t "after-add" in
  Lwt.return ()

let keys t =
  lwt fronts = fronts t in
  lwt backs = backs t in
  if Key.Set.is_empty fronts then
    Lwt.return []
  else
    lwt g = Key_store.keys (key_store t) ~sources:fronts ~sinks:backs () in
    let keys = Key.Graph.Topological.fold (fun key acc -> key :: acc) g [] in
    lwt keys = Lwt_list.fold_left_s (fun acc key ->
        lwt value = read_key t key in
        match Value.contents value with
        | None   -> Lwt.return acc
        | Some k -> Lwt.return (k :: acc)
      ) [] keys in
    Lwt.return (List.rev keys)

let to_list t =
  lwt keys = keys t in
  lwt () = dump t "to-list" in
  Lwt_list.fold_left_s (fun acc key ->
      lwt value = Value_store.read (value_store t) key in
      match value with
      | None   -> Lwt.return acc
      | Some v -> Lwt.return (v :: acc)
    ) [] keys

let peek t =
  lwt fronts = fronts t in
  try
    let key = Key.Set.choose fronts in
    read_contents t key
  with Not_found ->
    empty "FRONT"

let take t =
  lwt () = dump t "before-take" in
  lwt fronts = Tag_store.read (tag_store t) t.front in
  lwt backs  = Tag_store.read (tag_store t) t.back in
  lwt g = Key_store.keys (key_store t) ~sources:fronts ~sinks:backs () in
  lwt key =
    try Lwt.return (Key.Set.choose fronts)
    with Not_found -> empty "FRONT" in
  let fronts = Key.Set.remove key fronts in
  let move_front new_fronts =
    let new_fronts = Key.Set.union new_fronts fronts in
    lwt () =
      if Key.Set.is_empty new_fronts then Tag_store.remove (tag_store t) t.front
      else Tag_store.update (tag_store t) t.front new_fronts in
    lwt () = dump t "after-take" in
    read_contents t key in
  (* XXX: use a staging area *)
  match Key.Graph.succ g key with
  | []   -> move_front Key.Set.empty
  | keys ->
    lwt new_fronts = Lwt_list.filter_s (fun key ->
        lwt preds = Key_store.pred (key_store t) key in
        let todo = Key.Set.inter preds fronts in
        Lwt.return (Key.Set.is_empty todo)
      ) keys in
    move_front (Key.Set.of_list new_fronts)

let pull t ~origin =
  lwt () = dump origin "origin" in
  lwt remote_fronts = fronts origin in
  lwt remote_backs = backs origin in
  lwt local_fronts = fronts t in
  lwt local_backs = backs t in
  (* XXX: inefficient *)
  lwt remote_queue = keys origin in
  let sources = Key.Set.union remote_fronts (Key.Set.of_list remote_queue) in
  lwt g =
    Key_store.keys (key_store origin) ~sources ~sinks:remote_backs () in
  Key.Graph.dump g "pull";
  let keys = Key.Set.to_list (Key.Graph.vertex g) in
  (* Add new keys *)
  lwt () = Lwt_list.iter_s (fun k ->
      let preds = Key.Set.of_list (Key.Graph.pred g k) in
      Key_store.add (key_store t) k preds
    ) keys in
  (* Add a new merge commit, and update the 'back' tag to point to it *)
  let parents = Key.Set.union local_backs remote_backs in
  let merge_value = Value.revision None parents in
  lwt merge_key = Value_store.write (value_store t) merge_value in
  lwt () = Key_store.add (key_store t) merge_key parents in
  lwt () = Tag_store.update (tag_store t) t.back (Key.Set.singleton merge_key) in
  (* XXX: pulling values could be done on demand *)
  lwt () = Lwt_list.iter_s (fun k ->
      lwt v = Value_store.read (value_store origin) k in
      match v with
      | None   -> failwith "pull value"
      | Some v ->
        lwt local_k = Value_store.write (value_store t) v in
        assert (k = local_k);
        Lwt.return ()
    ) keys in
  (* Fix empty local queues *)
  lwt () =
    if not (Key.Set.is_empty local_fronts) then Lwt.return ()
    else Tag_store.update (tag_store t) t.front (Key.Graph.min g) in
  dump t "local"

let clone t ~origin =
  lwt () = init t in
  pull t ~origin

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
