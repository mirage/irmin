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

let invalid_page body =
  Server.respond_string ~status:`OK ~body ()

let respond_value value =
  let json = Memory.J.json_of_value value in
  let body = Memory.J.string_of_json json in
  Server.respond_string ~status:`OK ~body ()

let (>>=) x f =
  match x with
  | None   -> invalid_page "Invalid key"
  | Some x -> f x

let make_server t =

  let callback conn_id ?body req =
    let path = Request.path req in
    let path = Re_str.split_delim (Re_str.regexp_string "/") path in
    let path = List.filter ((<>) "") path in
    match path with
    | [] ->
      let keys = Memory.Low.list t in
      let json = Memory.J.json_of_keys keys in
      let body = Memory.J.string_of_json json in
      Server.respond_string ~status:`OK ~body ()
    | "tree" :: labels ->
      Memory.Tag.revision t (T "HEAD") >>= fun head ->
      let tree = Memory.Revision.tree t head in
      let labels = List.map (fun l -> L l) labels in
      Memory.Tree.get t tree labels >>= fun key ->
      Memory.Low.read t key >>=
      respond_value
    | [key] ->
      begin match Memory.Low.read t (Memory.Types.K key) with
        | None   -> invalid_page "Invalid key"
        | Some v -> respond_value v
      end
    | _ -> invalid_page "Invalid adress"
  in
  let conn_closed conn_id () =
    Printf.eprintf "conn %s closed\n%!" (Server.string_of_conn_id conn_id)
  in
  let config = { Server.callback; conn_closed } in
  server ~address:"127.0.0.1" ~port:8081 config

let _ =
  let t = Memory.create () in
  let root = Memory.save_dir t (Sys.getcwd ()) in
  match Memory.Low.read t root with
  | Some (Tree tree) ->
    let head = Memory.Revision.commit t [] tree in
    Memory.Tag.tag t (T "HEAD") head;
    Lwt_unix.run (make_server t)
  | _ -> failwith "init"
