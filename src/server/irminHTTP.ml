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
  let json = `O [ "result", json ] in
  let body = IrminJSON.output json in
  Server.respond_string ~status:`OK ~body ()

module Action (S: Irmin.S) = struct

  open S

  type 'a conv = {
    input : IrminJSON.t -> 'a;
    output: 'a -> IrminJSON.t;
  }

  let some fn = {
    input  = IrminJSON.to_option fn.input;
    output = IrminJSON.of_option fn.output
  }

  let list fn = {
    input  = IrminJSON.to_list fn.input;
    output = IrminJSON.of_list fn.output;
  }

  let bool = {
    input  = IrminJSON.to_bool;
    output = IrminJSON.of_bool;
  }

  let key = {
    input  = Key.of_json;
    output = Key.to_json;
  }

  let value = {
    input  = Value.of_json;
    output = Value.to_json;
  }

  let tree = {
    input  = Tree.of_json;
    output = Tree.to_json;
  }

  let revision = {
    input  = Revision.of_json;
    output = Revision.to_json;
  }

  let tag = {
    input  = Tag.of_json;
    output = Tag.to_json;
  }

  let path = {
    input  = IrminJSON.to_list IrminJSON.to_string;
    output = IrminJSON.of_list IrminJSON.of_string;
  }

  let unit = {
    input  = IrminJSON.to_unit;
    output = IrminJSON.of_unit;
  }

  let error msg =
    failwith ("error: " ^ msg)

  type t =
    | Leaf of (S.t -> IrminJSON.t list -> IrminJSON.t Lwt.t)
    | Node of (string * t) list

  let to_json t =
    let rec aux path acc = function
      | Leaf _ -> `String (String.concat "/" (List.rev path)) :: acc
      | Node c -> List.fold_left (fun acc (s,t) -> aux (s::path) acc t) acc c in
    `A (List.rev (aux [] [] t))

  let child c t: t =
    let error () =
      failwith ("Unknown action: " ^ c) in
    match t with
    | Leaf _ -> error ()
    | Node l ->
      try List.assoc c l
      with Not_found -> error ()

  let va t = t.value
  let tr t = t.tree
  let re t = t.revision
  let ta t = t.tag
  let t x = x

  let mk1 fn db i1 o =
    Leaf (fun t -> function
        | [x] ->
          let x = i1.input x in
          fn (db t) x >>= fun r ->
          return (o.output r)
        | []  -> error "Not enough arguments"
        | _   -> error "Too many arguments"
      )

  let mk2 fn db i1 i2 o =
    Leaf (fun t -> function
        | [x; y] ->
          let x = i1.input x in
          let y = i2.input y in
          fn (db t) x y >>= fun r ->
          return (o.output r)
        | [] | [_] -> error "Not enough arguments"
        | _        -> error "Too many arguments"
      )

  let value_store = Node [
      "read"  , mk1 Value.read va key   (some value);
      "mem"   , mk1 Value.mem  va key   bool;
      "list"  , mk1 Value.list va key   (list key);
      "add"   , mk1 Value.add  va value key;
  ]

  let tree_store = Node [
    "read"  , mk1 Tree.read tr key  (some tree);
    "mem"   , mk1 Tree.mem  tr key  bool;
    "list"  , mk1 Tree.list tr key  (list key);
    "add"   , mk1 Tree.add  tr tree key;
  ]

  let revision_store = Node [
    "read"  , mk1 Revision.read re key  (some revision);
    "mem"   , mk1 Revision.mem  re key  bool;
    "list"  , mk1 Revision.list re key  (list key);
    "add"   , mk1 Revision.add  re revision key;
  ]

  let tag_store = Node [
    "read"  , mk1 Tag.read   ta tag (some key);
    "mem"   , mk1 Tag.mem    ta tag bool;
    "list"  , mk1 Tag.list   ta tag (list tag);
    "update", mk2 Tag.update ta tag key unit;
    "remove", mk1 Tag.remove ta tag unit;
  ]

  let store = Node [
    "read"    , mk1 S.read    t path (some value);
    "mem"     , mk1 S.mem     t path bool;
    "list"    , mk1 S.list    t path (list path);
    "update"  , mk2 S.update  t path value unit;
    "remove"  , mk1 S.remove  t path unit;
    "value"   , value_store;
    "tree"    , tree_store;
    "revision", revision_store;
    "tag"     , tag_store;
  ]

end

let process (type t) (module S: Irmin.S with type t = t) (t:t) ?body path =
  let open S in
  let module Action = Action(S) in
  begin match body with
    | None   -> return_nil
    | Some b ->
      Body.string_of_body (Some b) >>= fun b ->
      match IrminJSON.input b with
      | `A l -> return l
      | _    -> failwith "Wrong parameters"
  end >>= fun params ->
  let rec aux actions path =
    match path with
    | []      -> respond_json (Action.to_json actions)
    | h::path ->
      match Action.child h actions with
      | Action.Leaf fn ->
        let params = match path with
          | [] -> params
          | _  -> (IrminJSON.of_strings path) :: params in
        fn t params >>= respond_json
      | actions -> aux actions path in
  aux Action.store path

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
