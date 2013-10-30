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
module Body = Cohttp_lwt_body

let respond body =
  Server.respond_string ~status:`OK ~body ()

let respond_json json =
  let json = match json with
    | `A _
    | `O _ -> json
    | _    -> `A [json] in
  let body = IrminJSON.output json in
  Server.respond_string ~status:`OK ~body ()

let respond_unit () =
  respond "OK"

let respond_bool = function
  | true -> respond_json (`A [`String "true"])
  | fale -> respond_json (`A [`String "false"])

let respond_strings strs =
  let json = `A (List.map (fun s -> `String s) strs) in
  respond_json json

let process (type t) (s: (module Irmin.S with type t = t)) (t:t) ?body path =
  let module S = (val s)in
  let open S in
  let respond_key key = respond_json (Key.to_json key) in
  let respond_value value = respond_json (Value.to_json value) in
  let respond_tree tree = respond_json (Tree.to_json tree) in
  let respond_revision rev = respond_json (Revision.to_json rev) in
  match path with
  | [] -> respond_strings [
      "action";
      "value";
      "tree";
      "revision";
      "tag";
    ]

  | ["action"] -> respond_strings [
      "update";
      "remove";
      "read";
      "mem";
      "list";
      "snapshot";
      "revert";
      "watch";
    ]

  (* ACTIONS *)

  | "action" :: "update" :: path ->
    begin match List.rev path with
      | [] | [_]      -> failwith "Wrong number of arguments"
      | value :: path ->
        update t (List.rev path) (Value.of_bytes value) >>= respond_unit
    end

  | "action" :: "remove" :: path ->
    remove t path >>= respond_unit

  | "action" :: "read" :: path ->
    read_exn t path >>= respond_value

  | "action" :: "mem" :: path ->
    mem t path >>= respond_bool

  | "action" :: "list" :: path ->
    list t path >>= fun paths ->
    let paths = List.map (String.concat "/") paths in
    respond_strings paths

  | ["action"; "snapshot"] ->
    snapshot t >>= respond_key

  | ["action"; "watch"] -> failwith "TODO"

  (* VALUES *)

  | ["value"; key] ->
    Value.read_exn t.value (Key.of_hex key) >>= respond_value

  (* TREE *)
  | ["tree"; key] ->
    Tree.read_exn t.tree (Key.of_hex key) >>= respond_tree

  (* REVISIONS *)
  | ["revision"; key] ->
    Revision.read_exn t.revision (Key.of_hex key) >>= respond_revision

  (* TAGS *)
  | ["tag"; tag] ->
    Tag.read_exn t.tag (Tag.of_string tag) >>= respond_key

  | _ -> failwith "Invalid URI"

let server s t port =
  Printf.printf "Irminsule server listening on port %d ...\n%!" port;
  let callback conn_id ?body req =
    let path = Uri.path (Request.uri req) in
    Printf.printf "Request received: PATH=%s\n%!" path;
    let path = Re_str.split_delim (Re_str.regexp_string "/") path in
    let path = List.filter ((<>) "") path in
    process s t ?body path in
  let conn_closed conn_id () =
    Printf.eprintf "Connection %s closed!\n%!" (Server.string_of_conn_id conn_id) in
  let config = { Server.callback; conn_closed } in
  Server.create ~address:"127.0.0.1" ~port config
