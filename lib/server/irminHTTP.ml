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

open Core_kernel.Std
open Lwt

module L = Log.Make(struct let section = "HTTP" end)

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

module Server (S: Irmin.S) = struct

  module K = S.Internal.Key
  let key = {
    input  = K.of_json;
    output = K.to_json;
  }

  module Blob = S.Internal.Blob
  module B = Blob.Value
  let blob = {
    input  = B.of_json;
    output = B.to_json;
  }

  module Tree = S.Internal.Tree
  module T = Tree.Value
  let tree = {
    input  = T.of_json;
    output = T.to_json;
  }

  module Commit = S.Internal.Commit
  module C = Commit.Value
  let commit = {
    input  = C.of_json;
    output = C.to_json;
  }

  module Reference = S.Reference
  module R = Reference.Key
  let reference = {
    input  = R.of_json;
    output = R.to_json;
  }

  module D = IrminDump.S(K)(B)

  let dump = {
    input  = D.of_json;
    output = D.to_json;
  }

  let contents key value =
    list (pair key value)

  let respond ?headers body =
    L.debugf "%S" body;
    Cohttp_lwt_unix.Server.respond_string ?headers ~status:`OK ~body ()

  let json_headers = Cohttp.Header.of_list [
      "Content-type", "application/json"
    ]

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
    let body = Cohttp_lwt_body.body_of_stream stream in
    Cohttp_lwt_unix.Server.respond ~headers:json_headers ~status:`OK ~body ()

  let error fmt =
    Printf.ksprintf (fun msg ->
        failwith ("error: " ^ msg)
      ) fmt

  type 'a leaf = S.t -> string list -> Ezjsonm.t option -> 'a

  type t =
    | Fixed  of Ezjsonm.t Lwt.t leaf
    | Stream of Ezjsonm.t Lwt_stream.t leaf
    | Node   of (string * t) list

  let to_json t =
    let rec aux path acc = function
      | Fixed   _
      | Stream _ -> `String (IrminPath.pretty (List.rev path)) :: acc
      | Node c   -> List.fold_left ~f:(fun acc (s,t) -> aux (s::path) acc t) ~init:acc c in
    `A (List.rev (aux [] [] t))

  let child c t: t =
    let error () =
      failwith ("Unknown action: " ^ c) in
    match t with
    | Fixed _
    | Stream _ -> error ()
    | Node l   ->
      try List.Assoc.find_exn l c
      with Not_found -> error ()

  let bl t = S.Internal.blob (S.internal t)
  let tr t = S.Internal.tree (S.internal t)
  let co t = S.Internal.commit (S.internal t)
  let re t = S.reference t
  let t x = x

  let mk0p name = function
    | [] -> ()
    | p  -> error "%s: non-empty path (%s)" name (IrminPath.to_string p)

  let mk0b name = function
    | None   -> ()
    | Some _ -> error "%s: non-empty body" name

  let mk1p name i path =
    match path with
    | [x] -> i.input (`String x)
    | []  -> error "%s: empty path" name
    | l   -> error "%s: %s is an invalid path" name (IrminPath.to_string l)

  let mk1b name i = function
    | None   -> error "%s: empty body" name
    | Some b  -> i.input b

  let mklp name i1 path =
    i1.input (Ezjsonm.strings path)

  (* no arguments, fixed answer *)
  let mk0p0bf name fn db o =
    name,
    Fixed (fun t path params ->
        mk0p name path;
        mk0b name params;
        fn (db t) >>= fun r ->
        return (o.output r)
      )

  (* 1 argument in the path, fixed answer *)
  let mk1p0bf name fn db i1 o =
    name,
    Fixed (fun t path params ->
        let x = mk1p name i1 path in
        mk0b name params;
        fn (db t) x >>= fun r ->
        return (o.output r)
      )

  (* list of arguments in the path, fixed answer *)
  let mklp0bf name fn db i1 o =
    name,
    Fixed (fun t path params ->
        let x = mklp name i1 path in
        mk0b name params;
        fn (db t) x >>= fun r ->
        return (o.output r)
      )

  (* 1 argument in the body *)
  let mk0p1bf name fn db i1 o =
    name,
    Fixed (fun t path params ->
        mk0p name path;
        let x = mk1b name i1 params in
        fn (db t) x >>= fun r ->
        return (o.output r)
      )

  (* 1 argument in the path, 1 argument in the body, fixed answer *)
  let mk1p1bf name fn db i1 i2 o =
    name,
    Fixed (fun t path params ->
        let x1 = mk1p name i1 path in
        let x2 = mk1b name i2 params in
        fn (db t) x1 x2 >>= fun r ->
        return (o.output r)
      )

  (* list of arguments in the path, 1 argument in the body, fixed answer *)
  let mklp1bf name fn db i1 i2 o =
    name,
    Fixed (fun t path params ->
        let x1 = mklp name i1 path in
        let x2 = mk1b name i2 params in
        fn (db t) x1 x2 >>= fun r ->
        return (o.output r)
      )

  (* list of arguments in the path, no body, streamed response *)
  let mklp0bs name fn db i1 o =
    name,
    Stream (fun t path params ->
        let x1 = mklp name i1 path in
        let stream = fn (db t) x1 in
        Lwt_stream.map (fun r -> o.output r) stream
      )

  let blob_store = Node [
      mk1p0bf "read"     Blob.read     bl key   (some blob);
      mk1p0bf "mem"      Blob.mem      bl key   bool;
      mk1p0bf "list"     Blob.list     bl key   (list key);
      mk0p1bf "add"      Blob.add      bl blob  key;
      mk0p0bf "contents" Blob.contents bl (contents key blob);
  ]

  let tree_store = Node [
      mk1p0bf "read"     Tree.read     tr key  (some tree);
      mk1p0bf "mem"      Tree.mem      tr key  bool;
      mk1p0bf "list"     Tree.list     tr key  (list key);
      mk0p1bf "add"      Tree.add      tr tree key;
      mk0p0bf "contents" Tree.contents tr (contents key tree);
  ]

  let commit_store = Node [
      mk1p0bf "read"     Commit.read     co key   (some commit);
      mk1p0bf "mem"      Commit.mem      co key   bool;
      mk1p0bf "list"     Commit.list     co key   (list key);
      mk0p1bf "add"      Commit.add      co commit key;
      mk0p0bf "contents" Commit.contents co (contents key commit);
  ]

  let reference_store = Node [
      mk1p0bf "read"     Reference.read     re reference (some key);
      mk1p0bf "mem"      Reference.mem      re reference bool;
      mk1p0bf "list"     Reference.list     re reference (list reference);
      mk1p1bf "update"   Reference.update   re reference key unit;
      mk1p0bf "remove"   Reference.remove   re reference unit;
      mk0p0bf "contents" Reference.contents re (contents reference key);
  ]

  let store = Node [
      mklp0bf "read"     S.read     t path (some blob);
      mklp0bf "mem"      S.mem      t path bool;
      mklp0bf "list"     S.list     t path (list path);
      mklp1bf "update"   S.update   t path blob unit;
      mklp0bf "remove"   S.remove   t path unit;
      mk0p0bf "contents" S.contents t (contents path blob);
      mk0p0bf "snapshot" S.snapshot t key;
      mk1p0bf "revert"   S.revert   t key unit;
      mklp0bf "export"   S.export   t (list key) dump;
      mk0p1bf "import"   S.import   t dump unit;
      mklp0bs "watch"    S.watch    t path (pair path key);
      "value" , blob_store;
      "tree"  , tree_store;
      "commit", commit_store;
      "ref"   , reference_store;
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
          L.debugf "process: length=%d body=%S" len b;
          try match Ezjsonm.from_string b with
            | `O l ->
              if List.Assoc.mem l "params" then
                return (List.Assoc.find l "params")
              else
                failwith "process: wrong request"
            | _    ->
              failwith "Wrong parameters"
          with _ ->
            L.debugf "process: not a valid JSON body %S" b;
            fail Invalid
        end
      | _ -> fail Invalid
    end >>= fun params ->
    let rec aux actions path =
      match path with
      | []      -> respond_json (to_json actions)
      | h::path ->
        match child h actions with
        | Fixed fn  -> fn t path params >>= respond_json
        | Stream fn -> respond_json_stream (fn t path params)
        | actions   -> aux actions path in
    aux store path

end

let start_server (type t) (module S: Irmin.S with type t = t) (t:t) uri =
  let port = match Uri.port uri with
    | None   -> 8080
    | Some p -> p in
  let module Server = Server(S) in
  L.debugf "start-server [port %d]" port;
  let callback conn_id ?body req =
    let path = Uri.path (Cohttp.Request.uri req) in
    L.debugf "Request received: PATH=%s" path;
    let path = String.split path ~on:'/' in
    let path = List.filter ~f:((<>) "") path in
    Server.process t ?body req path in
  let conn_closed conn_id () =
    L.debugf "Connection %s closed!" (Cohttp.Connection.to_string conn_id) in
  let config = { Cohttp_lwt_unix.Server.callback; conn_closed } in
  Cohttp_lwt_unix.Server.create ~address:"0.0.0.0" ~port config

let stop_server uri =
  let port = match Uri.port uri with
    | None   -> 8080
    | Some p -> p in
  L.debugf "stop-server [port %d]" port;
  Cohttp_lwt_unix_net.build_sockaddr "0.0.0.0" (string_of_int port) >>=
  fun sockaddr ->
  let sock =
    Lwt_unix.socket
      (Unix.domain_of_sockaddr sockaddr)
      Unix.SOCK_STREAM 0 in
  Lwt_unix.close sock
