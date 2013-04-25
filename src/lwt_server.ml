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

open Lwt
open Cohttp
open Cohttp_lwt_unix

open Memory.Types

let respond body =
  Server.respond_string ~status:`OK ~body ()

let respond_json json =
  let body = Memory.J.string_of_json json in
  Server.respond_string ~status:`OK ~body ()

let respond_value value =
  let json = Memory.J.json_of_value value in
  respond_json json

let respond_strings strs =
  let json = `A (List.map (fun s -> `String s) strs) in
  respond_json json

let respond_new_head (K key) =
  let body = Printf.sprintf "New HEAD: %s" key in
  Server.respond_string ~status:`OK ~body ()

let (>>=) x f =
  match x with
  | None   -> failwith "Invalid key"
  | Some x -> f x

let fmap f l =
  List.fold_left (fun l e ->
      match f e with
      | None   -> failwith "Invalid key"
      | Some x -> x::l
    ) [] l

let path = ref (Sys.getcwd ())
let exclude = ref None

let snapshot t =
  let root = Memory.save_dir t ?exclude:!exclude !path in
  Memory.Tree.read t root

let process t ?body = function
  | [] -> respond_strings [
            "action";
            "key";
            "revision";
            "tag";
            "tree"
          ]

  | ["action"] -> respond_strings [
                    "discover";
                    "pull";
                    "push";
                    "revert";
                    "commit";
                    "tag";
                    "watch";
                  ]

  | ["action"; "discover"] ->
    lwt body = Body.string_of_body body in
    let json = Memory.J.json_of_string body in
    let keys, tags = Memory.J.discover_of_json json in
    let keys = Memory.Remote.discover t keys tags in
    let json = Memory.J.json_of_keys keys in
    respond_json json

  | ["action"; "pull"] ->
    lwt body = Body.string_of_body body in
    let json = Memory.J.json_of_string body in
    let keys = Memory.J.keys_of_json json in
    let values = Memory.Remote.pull t keys in
    let json = Memory.J.json_of_values values in
    respond_json json

  | ["action"; "push"] ->
    lwt body = Body.string_of_body body in
    let json = Memory.J.json_of_string body in
    let values = Memory.J.values_of_json json in
    Memory.Remote.push t values;
    respond "OK"

  | ["action"; "revert"; key] ->
    let revision = match Memory.Low.read t (K key) with
      | Some (Revision r) -> r
      | _ -> match Memory.Tag.revision t (T key) with
        | Some r -> r
        | None   -> failwith "revert" in
    Memory.Tag.tag t (T "HEAD") revision;
    respond_new_head (K key)

  | ["action"; "snapshot"] ->
    snapshot t >>= fun tree ->
    Memory.Tag.revision t (T "HEAD") >>= fun head ->
    let new_head = Memory.Revision.commit t [head] tree in
    Memory.Tag.tag t (T "HEAD") new_head;
    let key = Memory.sha1 (Revision new_head) in
    respond_new_head key

  | ["action"; "tag"; tag] ->
    Memory.Tag.revision t (T "HEAD") >>= fun revision ->
    Memory.Tag.tag t (T tag) revision;
    respond (Printf.sprintf "new tag: %s" tag)

  | ["action"; "watch"] -> failwith "TODO"

  | ["key" ] ->
    let keys = Memory.Low.list t in
    let json = Memory.J.json_of_keys keys in
    respond_json json

  | ["key"; key] ->
    Memory.Low.read t (Memory.Types.K key) >>=
    respond_value

  | ["revision"] ->
    let keys = Memory.Revision.list t in
    let json = Memory.J.json_of_keys keys in
    respond_json json

  | ["revision"; key] ->
    Memory.Revision.read t (K key) >>= fun revision ->
    respond_value (Revision revision)

  | ["tag"] ->
    let tags = Memory.Tag.tags t in
    let json = Memory.J.json_of_tags tags in
    respond_json json

  | ["tag"; tag] ->
    Memory.Tag.revision t (T tag) >>= fun revision ->
    respond_value (Revision revision)

  | "tree" :: labels ->
    Memory.Tag.revision t (T "HEAD") >>= fun head ->
    Memory.Revision.tree t head >>= fun tree ->
    let labels = List.map (fun l -> L l) labels in
    Memory.Tree.get t tree labels >>= fun key ->
    Memory.Low.read t key >>=
    respond_value

  | _ -> failwith "Invalid URI"

let make_server t port =

  let callback conn_id ?body req =
    let path = Request.path req in
    let path = Re_str.split_delim (Re_str.regexp_string "/") path in
    let path = List.filter ((<>) "") path in
    process t ?body path in
  let conn_closed conn_id () =
    Printf.eprintf "conn %s closed\n%!" (Server.string_of_conn_id conn_id) in
  let config = { Server.callback; conn_closed } in
  server ~address:"127.0.0.1" ~port config

let init () =
  let t = Memory.create () in
  snapshot t >>= fun tree ->
  let head = Memory.Revision.commit t [] tree in
  Memory.Tag.tag t (T "HEAD") head;
  t

let start p port e =
  begin match p with
    | [p] -> path := p
    | _   -> failwith "Too many paths"
  end;
  begin match e with
    | [] -> ()
    | _  -> exclude := Some e;
  end;
  Lwt_unix.run (make_server (init ()) port)
