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

(*
 - 'front' events are the queue heads, it will be updated by [Queue.take]
   - 'back'  event is the queue tail, it will be updated by  [Queue.add]

   You can have multiple 'front's but only one 'back', because you
   have mulitple producer of events -- the eventual other irminsule
   instances -- but only one consummer (the local instance).

   An example of key graph:

    X -> .... -> Y
    |     /       |
    |    X        |
    |    |        |
  FRONT  |       BACK
       FRONT

   XXX: To optimize [Queue.take] we must add a 'staging' area where
   the distributed queue is nicely flattened and represented as a
   doubly-linked list.

*)

let front = Tag.of_name "FRONT"
let back = Tag.of_name "BACK"

exception Empty

type t =
  [ `Dir of string ]

type state = {
  disk: Disk.t;
  fronts: Key.Set.t;
  back  : Key.t option;
}

let init = function
  | `Dir f -> Disk.init f

let create = function
  | `Dir f ->
    let disk = Disk.create f in
    lwt fronts = Disk.Tag_store.read disk front in
    lwt back = Disk.Tag_store.read disk back in
    let back = match Key.Set.to_list back with
      | []  -> None
      | [k] -> Some k
      | _   -> failwith "multiple BACK" in
    Lwt.return { disk; fronts; back }

let is_empty t =
  Key.Set.is_empty t.fronts

let graph t =
  if is_empty t then Lwt.return (Graph.create ())
  else match t.back with
    | None      -> Lwt.return (Graph.create ())
    | Some back ->
      let roots = t.fronts in
      let sinks = Key.Set.singleton back in
      lwt g = Graph.of_store t.disk ~roots ~sinks () in
      Lwt.return g

(* ADD(y)

   Before:

     X -> .... -> Y
     |            |
   FRONT         BACK

   After:

     X -> .... -> Y -> y
     |                 |
   FRONT              BACK

*)

let add f value =
  lwt t = create f in
  lwt key = Disk.Value_store.write t.disk value in
  let parents = match t.back with
    | None   -> Key.Set.empty
    | Some k -> Key.Set.singleton k in
  let revision = Value.revision key parents in
  lwt new_back = Disk.Value_store.write t.disk revision in
  let new_back = Key.Set.singleton new_back in
  lwt () =
    if not (Key.Set.is_empty t.fronts) then Lwt.return ()
    else Disk.Tag_store.update t.disk front new_back in
  lwt () = Disk.Tag_store.update t.disk back new_back in
  lwt t = create f in
  lwt ga = Graph.of_store t.disk () in
  Graph.dump t.disk ga "ADD"

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

let to_list f =
  lwt t = create f in
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

(* TAKE() = x

  Before:

      X
        \
    x -> X -> .... -> Y
    |                 |
  FRONT              BACK

  After:

      X
        \
    x -> X -> .... -> Y
         |            |
       FRONT         BACK

*)

let peek f =
  lwt t = create f in
  match Key.Set.to_list t.fronts with
  | []     -> empty "empty FRONT"
  | key::_ -> contents t key

let take f =
  lwt t = create f in
  lwt key =
    try Lwt.return (Key.Set.choose t.fronts)
    with Not_found -> empty "FRONT" in
  let fronts = Key.Set.remove key t.fronts in
  let move_front new_fronts =
    let new_fronts = Key.Set.union new_fronts fronts in
    lwt () =
      if Key.Set.is_empty new_fronts then Disk.Tag_store.remove t.disk front
      else Disk.Tag_store.update t.disk front new_fronts in
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
  Lwt.return (is_empty t)
