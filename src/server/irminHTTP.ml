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

let debug fmt =
  IrminLog.debug "HTTP" fmt

type 'a t = {
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

let pair a b = {
  input  = IrminJSON.to_pair a.input b.input;
  output = IrminJSON.of_pair a.output b.output;
}

let bool = {
  input  = IrminJSON.to_bool;
  output = IrminJSON.of_bool;
}

let path = {
  input  = IrminJSON.to_list IrminJSON.to_string;
  output = IrminJSON.of_list IrminJSON.of_string;
}

let unit = {
  input  = IrminJSON.to_unit;
  output = IrminJSON.of_unit;
}

exception Invalid

module Server (S: Irmin.S) = struct

  let key = {
    input  = S.Key.of_json;
    output = S.Key.to_json;
  }

  let value = {
    input  = S.Value.of_json;
    output = S.Value.to_json;
  }

  let tree = {
    input  = S.Tree.of_json;
    output = S.Tree.to_json;
  }

  let revision = {
    input  = S.Revision.of_json;
    output = S.Revision.to_json;
  }

  let tag = {
    input  = S.Tag.of_json;
    output = S.Tag.to_json;
  }

  let dump = {
    input  = S.Dump.of_json;
    output = S.Dump.to_json;
  }

  let contents key value =
    list (pair key value)

  let respond body =
    debug "%S" body;
    Cohttp_lwt_unix.Server.respond_string ~status:`OK ~body ()

  let respond_json json =
    let json = `O [ "result", json ] in
    let body = IrminJSON.output json in
    respond body

  let error fmt =
    Printf.kprintf (fun msg ->
        failwith ("error: " ^ msg)
      ) fmt

  type t =
    | Leaf of (S.t -> string list -> IrminJSON.t option -> IrminJSON.t Lwt.t)
    | Node of (string * t) list

  let to_json t =
    let rec aux path acc = function
      | Leaf _ -> `String (IrminTree.Path.pretty (List.rev path)) :: acc
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

  let va = S.value_store
  let tr = S.tree_store
  let re = S.revision_store
  let ta = S.tag_store
  let t x = x

  let mk0p name = function
    | [] -> ()
    | p  -> error "%s: non-empty path (%s)" name (IrminTree.Path.to_string p)

  let mk0b name = function
    | None   -> ()
    | Some _ -> error "%s: non-empty body" name

  let mk1p name i path =
    match path with
    | [x] -> i.input (`String x)
    | []  -> error "%s: empty path" name
    | l   -> error "%s: %s is an invalid path" name (IrminTree.Path.to_string l)

  let mk1b name i = function
    | None   -> error "%s: empty body" name
    | Some b  -> i.input b

  let mklp name i1 path =
    i1.input (IrminJSON.of_strings path)

  (* no arguments *)
  let mk0p0b name fn db o =
    name,
    Leaf (fun t path params ->
        mk0p name path;
        mk0b name params;
        fn (db t) >>= fun r ->
        return (o.output r)
      )

  (* 1 argument in the path *)
  let mk1p0b name fn db i1 o =
    name,
    Leaf (fun t path params ->
        let x = mk1p name i1 path in
        mk0b name params;
        fn (db t) x >>= fun r ->
        return (o.output r)
      )

  (* list of arguments in the path *)
  let mklp0b name fn db i1 o =
    name,
    Leaf (fun t path params ->
        let x = mklp name i1 path in
        mk0b name params;
        fn (db t) x >>= fun r ->
        return (o.output r)
      )

  (* 1 argument in the body *)
  let mk0p1b name fn db i1 o =
    name,
    Leaf (fun t path params ->
        mk0p name path;
        let x = mk1b name i1 params in
        fn (db t) x >>= fun r ->
        return (o.output r)
      )

  (* 1 argument in the path, 1 argument in the body *)
  let mk1p1b name fn db i1 i2 o =
    name,
    Leaf (fun t path params ->
        let x1 = mk1p name i1 path in
        let x2 = mk1b name i2 params in
        fn (db t) x1 x2 >>= fun r ->
        return (o.output r)
      )

  (* list of arguments in the path, 1 argument in the body *)
  let mklp1b name fn db i1 i2 o =
    name,
    Leaf (fun t path params ->
        let x1 = mklp name i1 path in
        let x2 = mk1b name i2 params in
        fn (db t) x1 x2 >>= fun r ->
        return (o.output r)
      )


  let value_store = Node [
      mk1p0b "read"     S.Value.read     va key   (some value);
      mk1p0b "mem"      S.Value.mem      va key   bool;
      mk1p0b "list"     S.Value.list     va key   (list key);
      mk0p1b "add"      S.Value.add      va value key;
      mk0p0b "contents" S.Value.contents va (contents key value);
  ]

  let tree_store = Node [
      mk1p0b "read"     S.Tree.read     tr key  (some tree);
      mk1p0b "mem"      S.Tree.mem      tr key  bool;
      mk1p0b "list"     S.Tree.list     tr key  (list key);
      mk0p1b "add"      S.Tree.add      tr tree key;
      mk0p0b "contents" S.Tree.contents tr (contents key tree);
  ]

  let revision_store = Node [
      mk1p0b "read"     S.Revision.read     re key  (some revision);
      mk1p0b "mem"      S.Revision.mem      re key  bool;
      mk1p0b "list"     S.Revision.list     re key  (list key);
      mk0p1b "add"      S.Revision.add      re revision key;
      mk0p0b "contents" S.Revision.contents re (contents key revision);
  ]

  let tag_store = Node [
      mk1p0b "read"     S.Tag.read     ta tag (some key);
      mk1p0b "mem"      S.Tag.mem      ta tag bool;
      mk1p0b "list"     S.Tag.list     ta tag (list tag);
      mk1p1b "update"   S.Tag.update   ta tag key unit;
      mk1p0b "remove"   S.Tag.remove   ta tag unit;
      mk0p0b "contents" S.Tag.contents ta (contents tag key);
  ]

  let store = Node [
      mklp0b "read"     S.read     t path (some value);
      mklp0b "mem"      S.mem      t path bool;
      mklp0b "list"     S.list     t path (list path);
      mklp1b "update"   S.update   t path value unit;
      mklp0b "remove"   S.remove   t path unit;
      mk0p0b "contents" S.contents t (contents path value);
      mk0p0b "snapshot" S.snapshot t key;
      mk1p0b "revert"   S.revert   t key unit;
      mklp0b "export"   S.export   t (list key) dump;
      mk0p1b "import"   S.import   t dump unit;
      "value"   , value_store;
      "tree"    , tree_store;
      "revision", revision_store;
      "tag"     , tag_store;
  ]

  let process t ?body req path =
    begin match Cohttp.Request.meth req, body with
      | `DELETE ,_
      | `GET , _      -> return_none
      | `POST, Some b ->
        Cohttp_lwt_body.get_length body >>= fun (len, body) ->
        if len = 0 then
          return_none
        else begin
          Cohttp_lwt_body.string_of_body body >>= fun b ->
          debug "process: length=%d body=%S" len b;
          try match IrminJSON.input b with
            | `O l ->
              if List.mem_assoc "params" l then
                return (Some (List.assoc "params" l))
              else
                failwith "process: wrong request"
            | _    ->
              failwith "Wrong parameters"
          with _ ->
            debug "process: not a valid JSON body %S" b;
            fail Invalid
        end
      | _ -> fail Invalid
    end >>= fun params ->
    let rec aux actions path =
      match path with
      | []      -> respond_json (to_json actions)
      | h::path ->
        match child h actions with
        | Leaf fn -> fn t path params >>= respond_json
        | actions -> aux actions path in
    aux store path

end

let servers = Hashtbl.create 8

let start_server (type t) (module S: Irmin.S with type t = t) (t:t) uri =
  let port = match Uri.port uri with
    | None   -> 8080
    | Some p -> p in
  let module Server = Server(S) in
  debug "start-server [port %d]" port;
  let callback conn_id ?body req =
    let path = Uri.path (Cohttp.Request.uri req) in
    debug "Request received: PATH=%s" path;
    let path = IrminMisc.split path '/' in
    let path = List.filter ((<>) "") path in
    Server.process t ?body req path in
  let conn_closed conn_id () =
    debug "Connection %s closed!"
      (Cohttp_lwt_unix.Server.string_of_conn_id conn_id) in
  let config = { Cohttp_lwt_unix.Server.callback; conn_closed } in
  Cohttp_lwt_unix.Server.create ~address:"0.0.0.0" ~port config

let stop_server uri =
  let port = match Uri.port uri with
    | None   -> 8080
    | Some p -> p in
  debug "stop-server [port %d]" port;
  Cohttp_lwt_unix_net.build_sockaddr "0.0.0.0" port >>=
  fun sockaddr ->
  let sock =
    Lwt_unix.socket
      (Unix.domain_of_sockaddr sockaddr)
      Unix.SOCK_STREAM 0 in
  Lwt_unix.close sock
