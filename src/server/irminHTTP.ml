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

  let error msg =
    failwith ("error: " ^ msg)

  type t =
    | Leaf of (S.t -> IrminJSON.t list -> IrminJSON.t Lwt.t)
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

  let va t = t.S.value
  let tr t = t.S.tree
  let re t = t.S.revision
  let ta t = t.S.tag
  let t x = x

  let mk0 fn db o =
    Leaf (fun t -> function
        | [] ->
          fn (db t) >>= fun r ->
          return (o.output r)
        | _ -> error "Too many arguments"
      )

  let mk1 fn db i1 o =
    Leaf (fun t -> function
        | [x] ->
          let x = i1.input x in
          fn (db t) x >>= fun r ->
          return (o.output r)
        | []  -> error "Not enough arguments"
        | _   -> error "Too many arguments"
      )

  let mkl fn db i1 o =
    Leaf (fun t l ->
        let x = i1.input (`A l) in
        fn (db t) x >>= fun r ->
        return (o.output r)
      )

  let mks fn db i1 o =
    Leaf (fun t l ->
        let x = List.map i1.input l in
        fn (db t) x >>= fun r ->
        return (o.output r)
      )

  let mkl2 fn db i1 i2 o =
    Leaf (fun t l ->
        match List.rev l with
        | []     -> error "Not enough arguments"
        | x2::x1 ->
          let x2 = i2.input x2 in
          let x1 = i1.input (`A (List.rev x1)) in
          fn (db t) x1 x2 >>= fun r ->
          return (o.output r)
      )

  let mko fn db i1 o =
    Leaf (fun t l ->
        let x = match l with
          | []  -> None
          | [x] -> Some (i1.input x)
          | _   -> error "Too many arguments" in
        fn (db t) x >>= fun r ->
        return (o.output r)
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
      "read"    , mk1 S.Value.read     va key   (some value);
      "mem"     , mk1 S.Value.mem      va key   bool;
      "list"    , mk1 S.Value.list     va key   (list key);
      "add"     , mk1 S.Value.add      va value key;
      "contents", mk0 S.Value.contents va (contents key value);
  ]

  let tree_store = Node [
    "read"    , mk1 S.Tree.read     tr key  (some tree);
    "mem"     , mk1 S.Tree.mem      tr key  bool;
    "list"    , mk1 S.Tree.list     tr key  (list key);
    "add"     , mk1 S.Tree.add      tr tree key;
    "contents", mk0 S.Tree.contents tr (contents key tree);
  ]

  let revision_store = Node [
    "read"    , mk1 S.Revision.read     re key  (some revision);
    "mem"     , mk1 S.Revision.mem      re key  bool;
    "list"    , mk1 S.Revision.list     re key  (list key);
    "add"     , mk1 S.Revision.add      re revision key;
    "contents", mk0 S.Revision.contents re (contents key revision);
  ]

  let tag_store = Node [
    "read"    , mk1 S.Tag.read     ta tag (some key);
    "mem"     , mk1 S.Tag.mem      ta tag bool;
    "list"    , mk1 S.Tag.list     ta tag (list tag);
    "update"  , mk2 S.Tag.update   ta tag key unit;
    "remove"  , mk1 S.Tag.remove   ta tag unit;
    "contents", mk0 S.Tag.contents ta (contents tag key);
  ]

  let store = Node [
    "read"    , mkl  S.read     t path (some value);
    "mem"     , mkl  S.mem      t path bool;
    "list"    , mkl  S.list     t path (list path);
    "update"  , mkl2 S.update  t path value unit;
    "remove"  , mkl  S.remove   t path unit;
    "contents", mk0  S.contents t (contents path value);
    "export"  , mks  S.export   t key dump;
    "import"  , mk1  S.import   t dump unit;
    "value"   , value_store;
    "tree"    , tree_store;
    "revision", revision_store;
    "tag"     , tag_store;
  ]

  let process t ?body req path =
    begin match Cohttp.Request.meth req, body with
      | `DELETE ,_
      | `GET , _      -> return_nil
      | `POST, Some b ->
        Cohttp_lwt_body.get_length body >>= fun (len, body) ->
        if len = 0 then
          return_nil
        else begin
          Cohttp_lwt_body.string_of_body body >>= fun b ->
          debug "process: length=%d body=%S" len b;
          try match IrminJSON.input b with
            | `A l -> return l
            | _    -> failwith "Wrong parameters"
          with e ->
            debug "process: wrong body %s" (Printexc.to_string e);
            fail Invalid
        end
      | _ -> fail Invalid
    end >>= fun params ->
    let rec aux actions path =
      match path with
      | []      -> respond_json (to_json actions)
      | h::path ->
        match child h actions with
        | Leaf fn ->
          let params = match path with
            | [] -> params
            | _  -> List.map IrminJSON.of_string path @ params in
          fn t params >>= respond_json
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
