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

module Graph = IrminKey.Graph(Disk.Key_store)(Disk.Value_store)(Disk.Tag_store)

exception Empty

let default_front = Tag.of_name "FRONT"
let default_back = Tag.of_name "BACK"

type t = {
  source: [ `Dir of string ];
  front : Tag.t;
  back  : Tag.t;
  disk  : Disk.t;
}

let init { source = `Dir f } =
  Disk.init f

let create ?(front = default_front) ?(back = default_back) source =
  let disk = match source with `Dir f -> Disk.create f in
  { source; front; back; disk }

let is_empty t =
  lwt fronts = Disk.Tag_store.read t.disk t.front in
  lwt backs  = Disk.Tag_store.read t.disk t.back in
  Lwt.return (Key.Set.is_empty fronts || Key.Set.is_empty backs)

let graph t =
  lwt fronts = Disk.Tag_store.read t.disk t.front in
  lwt backs  = Disk.Tag_store.read t.disk t.back in
  if Key.Set.is_empty fronts || Key.Set.is_empty backs then
    Lwt.return (Graph.create ())
  else
    Graph.of_store t.disk ~roots:fronts ~sinks:backs ()

let add t value =
  lwt key = Disk.Value_store.write t.disk value in
  lwt fronts = Disk.Tag_store.read t.disk t.front in
  lwt backs  = Disk.Tag_store.read t.disk t.back in
  let revision = Value.revision key backs in
  lwt new_back = Disk.Value_store.write t.disk revision in
  let new_back = Key.Set.singleton new_back in
  lwt () =
    if not (Key.Set.is_empty fronts) then Lwt.return ()
    else Disk.Tag_store.update t.disk t.front new_back in
  Disk.Tag_store.update t.disk t.back new_back

let empty fmt =
  Printf.kprintf (fun str ->
      IrminMisc.error "QUEUE" "%s" str;
      raise_lwt Empty
    ) fmt

let value_of_key t key =
  lwt v = Disk.Value_store.read t.disk key in
  match v with
  | None   -> empty "No value!"
  | Some v -> Lwt.return v

let contents t key =
  lwt v = value_of_key t key in
  match Value.contents v with
  | None   -> empty "No content!"
  | Some k -> value_of_key t k

let to_list t =
  lwt ga = Graph.of_store t.disk () in
  lwt () = Graph.dump t.disk ga "ALL" in
  lwt g = graph t in
  lwt () = Graph.dump t.disk g "FRONTS" in
  let keys = Graph.Topological.fold (fun key acc -> key :: acc) g [] in
  lwt values = Lwt_list.fold_left_s (fun acc key ->
      lwt value = contents t key in
      Lwt.return (value :: acc)
    ) [] keys in
  Lwt.return values

let peek t =
  lwt fronts = Disk.Tag_store.read t.disk t.front in
  try
    let key = Key.Set.choose fronts in
    contents t key
  with Not_found ->
    empty "FRONT"

let take t =
  lwt fronts = Disk.Tag_store.read t.disk t.front in
  lwt key =
    try Lwt.return (Key.Set.choose fronts)
    with Not_found -> empty "FRONT" in
  let fronts = Key.Set.remove key fronts in
  let move_front new_fronts =
    let new_fronts = Key.Set.union new_fronts fronts in
    lwt () =
      if Key.Set.is_empty new_fronts then Disk.Tag_store.remove t.disk t.front
      else Disk.Tag_store.update t.disk t.front new_fronts in
    contents t key in
  (* XXX: use a staging area *)
  lwt g = graph t in
  match Graph.succ g key with
  | []   -> move_front Key.Set.empty
  | keys ->
    lwt new_fronts = Lwt_list.filter_s (fun key ->
        lwt preds = Disk.Key_store.pred t.disk key in
        let todo = Key.Set.inter preds fronts in
        Lwt.return (Key.Set.is_empty todo)
      ) keys in
    move_front (Key.Set.of_list new_fronts)

let watch _ =
  failwith "TODO"

let pull _ =
  failwith "TODO"

let push _ =
  failwith "TODO"

let clone _ =
  failwith "TODO"
