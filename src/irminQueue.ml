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
    let head = match Key.Set.to_list head with
      | []  -> None
      | [k] -> Some k
      | _   -> failwith "head" in
    let tail = match Key.Set.to_list tail with
      | []  -> None
      | [k] -> Some k
      | _   -> failwith "tail" in
    Lwt.return { disk; head; tail }

let is_empty t =
  debug "is-empty";
  match t.head, t.tail with
  | None  , None   -> Lwt.return true
  | Some _, Some _ -> Lwt.return false
  | _ -> failwith "is_empty"

(* ADD(y)

  Before:

    X -> .... -> Y
    |            |
   HEAD         TAIL

  After:

    X -> .... -> Y -> y
    |            |
   HEAD         TAIL

*)
let add_one f value =
  lwt t = create f in
  lwt key = Disk.Value_store.write t.disk value in
  let parents = match t.tail with
    | None   -> Key.Set.empty
    | Some k -> Key.Set.singleton k in
  let revision = Value.revision key parents in
  lwt new_tail = Disk.Value_store.write t.disk revision in
  let new_tail = Key.Set.singleton new_tail in
  lwt () = match t.head with
    | None   -> Disk.Tag_store.update t.disk head new_tail
    | Some k -> Lwt.return () in
  Disk.Tag_store.update t.disk tail new_tail

let add f = function
  | []     -> Lwt.return ()
  | values -> Lwt_list.iter_s (add_one f) values

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

let peek f =
  lwt t = create f in
  match t.head with
  | None     -> empty "No head!"
  | Some key -> contents t key

module Graph = IrminKey.Graph(Disk.Key_store)(Disk.Value_store)(Disk.Tag_store)

let graph t =
  match t.head, t.tail with
  | Some head, Some tail ->
    let roots = Key.Set.singleton head in
    let sinks = Key.Set.singleton tail in
    lwt g = Graph.of_store t.disk ~roots ~sinks () in
    Lwt.return (Some g)
  | _ ->
    Lwt.return None

let to_list f =
  lwt t = create f in
  lwt ga = Graph.of_store t.disk () in
  lwt () = Graph.dump t.disk ga "ALL" in
  lwt g = graph t in
  match g with
  | None   -> Lwt.return []
  | Some g ->
    lwt () = Graph.dump t.disk g "HEAD" in
    let keys = Graph.Topological.fold (fun key acc -> key :: acc) g [] in
    lwt values = Lwt_list.fold_left_s (fun acc key ->
        lwt value = contents t key in
        Lwt.return (value :: acc)
      ) [] keys in
    Lwt.return values

(* TAKE() = x

  Before:

    x -> X -> .... -> Y
    |                 |
   HEAD              TAIL

  After:

    x -> X -> .... -> Y
         |            |
        HEAD         TAIL

*)

(* XXX: that a bit too expensive as you have to reconstruct the graph
   of keys to compute the successors ... *)
let take f =
  lwt t = create f in
  match t.head with
  | None     -> empty "No tail!"
  | Some key ->
    let move_head new_head =
      let new_head = Key.Set.singleton new_head in
      lwt () = Disk.Tag_store.update t.disk head new_head in
      contents t key in
    let empty () =
      lwt () = Disk.Tag_store.remove t.disk head in
      lwt () = Disk.Tag_store.remove t.disk tail in
      contents t key in
    lwt g = graph t in
    match g with
    | None   -> failwith "take"
    | Some g ->
      match Graph.succ g key with
      | []         -> empty ()
      | [new_head] -> move_head new_head
      | k::keys    ->
        (* XXX: need to rewrite the history to insert k before all the [keys] *)
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
