(*
 * Copyright (c) 2013-2014 Thomas Gazagnaire <thomas@gazagnaire.org>
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

let error fmt =
  Printf.ksprintf (fun msg ->
      failwith ("error: " ^ msg)
    ) fmt

module Log = Log.Make(struct let section = "HTTP" end)

type 'a t = {
  input : Ezjsonm.t -> 'a;
  output: 'a -> Ezjsonm.t;
}

let some fn = {
  input  = Ezjsonm.get_option fn.input;
  output = Ezjsonm.option fn.output
}

let list fn = {
  input  = Ezjsonm.get_list fn.input;
  output = Ezjsonm.list fn.output;
}

let pair a b = {
  input  = Ezjsonm.get_pair a.input b.input;
  output = Ezjsonm.pair a.output b.output;
}

let bool = {
  input  = Ezjsonm.get_bool;
  output = Ezjsonm.bool;
}

let path = {
  input  = Ezjsonm.get_list Ezjsonm.get_string;
  output = Ezjsonm.list Ezjsonm.string;
}

let unit = {
  input  = Ezjsonm.get_unit;
  output = (fun () -> Ezjsonm.unit);
}

exception Invalid

module type S = sig
  type t
  val listen: t -> ?timeout:int -> Uri.t -> unit Lwt.t
end

module type SERVER = sig
  include Cohttp_lwt.Server
  val listen: t -> ?timeout:int -> Uri.t -> unit Lwt.t
end

module Make (HTTP: SERVER)
    (S: Irmin.Private.Contents) = struct

  module K = S.Key
  let key = {
    input  = K.of_json;
    output = K.to_json;
  }

  module V = S.Val
  let contents = {
    input  = V.of_json;
    output = V.to_json;
  }

  module Node = S.Block.Node
  module T = Node.Value
  let node = {
    input  = T.of_json;
    output = T.to_json;
  }

  module Commit = S.Block.Commit
  module C = Commit.Value
  let commit = {
    input  = C.of_json;
    output = C.to_json;
  }

  module Tag = S.Tag
  module TK = Tag.Key
  let tag = {
    input  = TK.of_json;
    output = TK.to_json;
  }

  let origin = {
    input  = IrminOrigin.of_json;
    output = IrminOrigin.to_json;
  }

  let result = {
    input  = IrminMerge.UnitResult.of_json;
    output = IrminMerge.UnitResult.to_json;
  }

  let mk_dump key value =
    list (pair key value)

  let read_exn file =
    match IrminHTTPStatic.read file with
    | None   -> error "%s: not found" file
    | Some s -> s

  let respond_error e =
    let json = `O [ "error", IrminMisc.json_encode (Exn.to_string e) ] in
    let body = Ezjsonm.to_string json in
    HTTP.respond_string
      ~headers:json_headers
      ~status:`Internal_server_error
      ~body ()

  let respond ?headers body =
    Log.debug
      (lazy (String.escaped @@
             if String.length body > 140 then
               String.sub body 0 100 ^ ".."
             else body));
    HTTP.respond_string ?headers ~status:`OK ~body ()

  let respond_json json =
    let json = `O [ "result", json ] in
    let body = Ezjsonm.to_string json in
    respond ~headers:json_headers body

  let respond_json_stream stream =
    let (++) = Lwt_stream.append in
    let stream =
      (Lwt_stream.of_list ["["])
      ++ (Lwt_stream.map (fun j -> Ezjsonm.to_string (`O ["result", j]) ^ ",") stream)
      ++ (Lwt_stream.of_list [" {\"result\":[]}]"])
    in
    let body = Cohttp_lwt_body.of_stream stream in
    HTTP.respond ~headers:json_headers ~status:`OK ~body ()

  type 'a leaf = S.t -> string list -> Ezjsonm.t option -> (string * string list) list -> 'a

  type response =
    | Fixed  of Ezjsonm.t Lwt.t leaf
    | Stream of Ezjsonm.t Lwt_stream.t leaf
    | Node   of (string * response) list
    | Html   of string Lwt.t leaf

  let to_json t =
    let rec aux path acc = function
      | Fixed   _
      | Stream _
      | Html _   -> `String (IrminPath.to_string (List.rev path)) :: acc
      | Node c   -> List.fold_left c
                      ~f:(fun acc (s,t) -> aux (s::path) acc t)
                      ~init:acc in
    `A (List.rev (aux [] [] t))

  let child c t: response =
    let error () =
      failwith ("Unknown action: " ^ c) in
    match t with
    | Fixed _
    | Stream _ -> error ()
    | Html _   -> t
    | Node l   ->
      try List.Assoc.find_exn l c
      with Not_found -> error ()

  let t x = x

  let mk0p name = function
    | [] -> ()
    | p  -> error "%s: non-empty path (%s)" name (IrminPath.to_string p)

  let mk0b name = function
    | None   -> ()
    | Some _ -> error "%s: non-empty body" name

  let mk0q name = function
    | [] -> ()
    | _  -> error "%s: non-empty query" name

  let mk1p name i path =
    match path with
    | [x] -> i.input (`String x)
    | []  -> error "%s: empty path" name
    | l   -> error "%s: %s is an invalid path" name (IrminPath.to_string l)

  let mk1b name i = function
    | None   -> error "%s: empty body" name
    | Some b  -> i.input b

  let mk2b name i j = function
    | None   -> error "%s: empty body" name
    | Some b -> (pair i j).input b

  let mklp name i1 path =
    i1.input (Ezjsonm.strings path)

  (* no arguments, fixed answer *)
  let mk0p0bf name fn db o =
    name,
    Fixed (fun t path params query ->
        mk0p name path;
        mk0b name params;
        mk0q name query;
        fn (db t) >>= fun r ->
        return (o.output r)
      )

  (* 0 argument, return an html page. *)
  let mk0p0bh name fn db =
    name,
    Html (fun t path params query ->
        mk0b name params;
        fn (db t) path query
      )

  (* 1 argument in the path, fixed answer *)
  let mk1p0bf name fn db i1 o =
    name,
    Fixed (fun t path params query ->
        let x = mk1p name i1 path in
        mk0b name params;
        mk0q name query;
        fn (db t) x >>= fun r ->
        return (o.output r)
      )

  (* list of arguments in the path, fixed answer *)
  let mklp0bf name fn db i1 o =
    name,
    Fixed (fun t path params query ->
        let x = mklp name i1 path in
        mk0b name params;
        mk0q name query;
        fn (db t) x >>= fun r ->
        return (o.output r)
      )

  (* list of arguments in the path, query strings, fixed answer *)
  let mklp0b1qf name fn db i1 o =
    name,
    Fixed (fun t path params query ->
        let x = mklp name i1 path in
        mk0b name params;
        fn (db t) x query >>= fun r ->
        return (o.output r)
      )

  (* 1 argument in the body *)
  let mk0p1bf name fn db i1 o =
    name,
    Fixed (fun t path params query ->
        mk0p name path;
        mk0q name query;
        let x = mk1b name i1 params in
        fn (db t) x >>= fun r ->
        return (o.output r)
      )

  (* 1 argument in the path, 1 argument in the body, fixed answer *)
  let mk1p1bf name fn db i1 i2 o =
    name,
    Fixed (fun t path params query ->
        let x1 = mk1p name i1 path in
        let x2 = mk1b name i2 params in
        mk0q name query;
        fn (db t) x1 x2 >>= fun r ->
        return (o.output r)
      )

  (* list of arguments in the path, 1 argument in the body, fixed answer *)
  let mklp1bf name fn db i1 i2 o =
    name,
    Fixed (fun t path params query ->
        let x1 = mklp name i1 path in
        let x2 = mk1b name i2 params in
        mk0q name query;
        fn (db t) x1 x2 >>= fun r ->
        return (o.output r)
      )

  (* list of arguments in the path, 2 arguments in the body, fixed answer *)
  let mklp2bf name fn db i1 i2 i3 o =
    name,
    Fixed (fun t path params query ->
        let x1 = mklp name i1 path in
        let x2, x3 = mk2b name i2 i3 params in
        mk0q name query;
        fn (db t) x1 x2 x3 >>= fun r ->
        return (o.output r)
      )

  (* list of arguments in the path, no body, streamed response *)
  let mklp0bs name fn db i1 o =
    name,
    Stream (fun t path params query ->
        let x1 = mklp name i1 path in
        mk0q name query;
        let stream = fn (db t) x1 in
        Lwt_stream.map (fun r -> o.output r) stream
      )

  let graph_index = read_exn "index.html"

  let graph_of_dump (dump:S.Sync.db) path query =
    match path with
    | [] -> return graph_index
    | ["graph.dot"] ->
      let depth = match List.Assoc.find query "depth" with
        | Some (x::_) -> Some (Int.of_string x)
        | _           -> None in
      let full = match List.Assoc.find query "full" with
        | Some (x::_) -> Some (x <> "0")
        | _           -> None in
      let buffer = Buffer.create 1024 in
      S.Dump.output_buffer dump ?depth ?full buffer >>= fun () ->
      let str = Buffer.contents buffer in
      (* Fix the OCamlGraph output (XXX: open an issue upstream) *)
      let str = IrminMisc.replace ~pattern:",[ \\n]*]" (fun _ -> "]") str in
      return str
    | [file] -> mk0q "dump" query; return (read_exn file)
    | l      -> error "%s: not found" (String.concat ~sep:"/" l)

  let contents_store = Node [
      mk1p0bf "read" Contents.read S.contents_t key (some contents);
      mk1p0bf "mem"  Contents.mem  S.contents_t key bool;
      mklp0bf "list" Contents.list S.contents_t (list key) (list key);
      mk0p1bf "add"  Contents.add  S.contents_t contents key;
      mk0p0bf "dump" Contents.dump S.contents_t (mk_dump key contents);
  ]

  let node_store = Node [
      mk1p0bf "read" Node.read S.node_t key (some node);
      mk1p0bf "mem"  Node.mem  S.node_t key bool;
      mklp0bf "list" Node.list S.node_t (list key) (list key);
      mk0p1bf "add"  Node.add  S.node_t node key;
      mk0p0bf "dump" Node.dump S.node_t (mk_dump key node);
  ]

  let commit_store =
    let commit_list t keys query =
      let depth =
        match List.Assoc.find query "depth" with
        | Some [d] -> Some (Int.of_string d)
        | _        -> None in
      Commit.list t ?depth keys in
    Node [
      mk1p0bf   "read" Commit.read S.commit_t key (some commit);
      mk1p0bf   "mem"  Commit.mem  S.commit_t key bool;
      mklp0b1qf "list" commit_list S.commit_t (list key) (list key);
      mk0p1bf   "add"  Commit.add  S.commit_t commit key;
      mk0p0bf   "dump" Commit.dump S.commit_t (mk_dump key commit);
  ]

  let tag_store = Node [
      mklp0bf "read"   Tag.read   S.tag_t tag (some key);
      mklp0bf "mem"    Tag.mem    S.tag_t tag bool;
      mklp0bf "list"   Tag.list   S.tag_t (list tag) (list tag);
      mklp1bf "update" Tag.update S.tag_t tag key unit;
      mklp0bf "remove" Tag.remove S.tag_t tag unit;
      mk0p0bf "dump"   Tag.dump   S.tag_t (mk_dump tag key);
      mklp0bs "watch"  Tag.watch  S.tag_t tag key;
  ]

  let store =
    let s_update t p o c = S.update t ?origin:o p c in
    let s_remove t p o = S.remove t ?origin:o p in
    let s_merge t b o = S.merge t ?origin:o b in
    let s_dump t = graph_of_dump t in (* XXX: weird API *)
    Node [
      mklp0bf "read"     S.read     t path (some contents);
      mklp0bf "mem"      S.mem      t path bool;
      mk0p1bf "list"     S.list     t (list path) (list path);
      mklp2bf "update"   s_update   t path (some origin) contents unit;
      mklp1bf "remove"   s_remove   t path (some origin) unit;
      mk0p0bh "dump"     s_dump     t;
      mklp0bs "watch"    S.watch    t path contents;
      mklp1bf "merge"    s_merge    t tag (some origin) result;
      "contents", contents_store;
      "node"    , node_store;
      "commit"  , commit_store;
      "tag"     , tag_store;
  ]

  let process t req body path =
    begin match Cohttp.Request.meth req with
      | `DELETE
      | `GET       -> return_none
      | `POST ->
        Cohttp_lwt_body.length body >>= fun (len, body) ->
        if len = 0 then
          return_none
        else begin
          Cohttp_lwt_body.to_string body >>= fun b ->
          Log.debugf "process: length=%d body=%S" len b;
          try match Ezjsonm.from_string b with
            | `O l ->
              if List.Assoc.mem l "params" then
                return (List.Assoc.find l "params")
              else
                failwith "process: wrong request"
            | _    ->
              failwith "Wrong parameters"
          with _ ->
            Log.debugf "process: not a valid JSON body %S" b;
            fail Invalid
        end
      | _ -> fail Invalid
    end >>= fun params ->
    let query = Uri.query (Cohttp.Request.uri req) in
    let rec aux actions path =
      match path with
      | []      -> respond_json (to_json actions)
      | h::path ->
        match child h actions with
        | Fixed fn  -> fn t path params query >>= respond_json
        | Stream fn -> respond_json_stream (fn t path params query)
        | Html fn   -> fn t path params query >>= respond
        | actions   -> aux actions path in
    aux store path

  type t = S.t

  let listen t ?timeout uri =
    let uri = match Uri.host uri with
      | None   -> Uri.with_host uri (Some "localhost")
      | Some _ -> uri in
    let port, uri = match Uri.port uri with
      | None   -> 8080, Uri.with_port uri (Some 8080)
      | Some p -> p   , uri in
    printf "Server started on port %d.\n%!" port;
    let callback conn_id req body =
      let path = Uri.path (Cohttp.Request.uri req) in
      Log.infof "Request received: PATH=%s" path;
      let path = String.split path ~on:'/' in
      let path = List.filter ~f:((<>) "") path in
      catch
        (fun () -> process t req body path)
        (fun e  -> respond_error e) in
    let conn_closed conn_id () =
      Log.debugf "Connection %s closed!" (Cohttp.Connection.to_string conn_id) in
    let config = { HTTP.callback; conn_closed } in
    HTTP.listen config ?timeout uri

end
