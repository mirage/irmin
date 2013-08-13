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

open IrminImpl

module Client = struct

  let add _ =
    failwith "TODO"

  let watch _ =
    failwith "TODO"

  let take _ =
    failwith "TODO"

  let peek _ =
    failwith "TODO"

  let dump _ =
    failwith "TODO"

  let pull _ =
    failwith "TODO"

  let push _ =
    failwith "TODO"

  let clone _ =
    failwith "TODO"

end

module Server = struct

  let init _ =
    failwith "TODO"

end
(*
module Body = Cohttp_lwt_body

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

let respond_new_head key =
  let body = Printf.sprintf "New HEAD: %s" (string_of_key key) in
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

let process t ?body = function
  | [] -> respond_strings [
      "action";
      "key";
      "revision";
      "tag";
      "tree"
    ]

  | ["action"] -> respond_strings [
      "dump";
      "pull";
      "push";
      "snapshot";
      "revert";
      "key";
      "revision";
      "tag";
      "add";
      "take";
    ]

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

  | ["action"; "revert"; hash] ->
    let key = K hash in
    let revision = match Memory.Low.read t key with
      | Some (Revision r) -> r
      | _ -> match Memory.Tag.revision t (T hash) with
        | Some r -> r
        | None   -> failwith "revert" in
    Memory.Tag.tag t (T "HEAD") revision;
    respond_new_head key

  | ["action"; "add"] ->
    lwt body = Body.string_of_body body in
    let _, tree = Memory.save_queue t body in
    Memory.Tag.revision t (T "HEAD") >>= fun head ->
    let new_head = Memory.Revision.commit t [head] tree in
    Memory.Tag.tag t (T "HEAD") new_head;
    let key = Memory.Types.key (Revision new_head) in
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

  | ["key"; hash] ->
    let key = K hash in
    Memory.Low.read t key >>=
    respond_value

  | ["revision"] ->
    let keys = Memory.Revision.list t in
    let json = Memory.J.json_of_keys keys in
    respond_json json

  | ["revision"; hash] ->
    let key = K hash in
    Memory.Revision.read t key >>= fun revision ->
    respond_value (Revision revision)

  | ["tag"] ->
    let tags = Memory.Tag.tags t in
    let json = Memory.J.json_of_tags tags in
    respond_json json

  | ["tag"; tag] ->
    Memory.Tag.revision t (T tag) >>= fun revision ->
    respond_value (Revision revision)

  | ["dump"; "queue"] ->
    Memory.Commit.read

  | _ -> failwith "Invalid URI"

let make_server t port =
  Printf.printf "Irminsule server listening on port %d ...\n%!" port;
  let callback conn_id ?body req =
    let path = Uri.path (Request.uri req) in
    Printf.printf "Request received: PATH=%s\n%!" path;
    let path = Re_str.split_delim (Re_str.regexp_string "/") path in
    let path = List.filter ((<>) "") path in
    process t ?body path in
  let conn_closed conn_id () =
    Printf.eprintf "Connection %s closed!\n%!" (Server.string_of_conn_id conn_id) in
  let config = { Server.callback; conn_closed } in
  Server.create ~address:"127.0.0.1" ~port config

let init port =
  let t = Memory.create () in
  let _, tree = Memory.init_queue t in
  let head = Memory.Revision.commit t [] tree in
  Memory.Tag.tag t (T "HEAD") head;
  Lwt_unix.run (make_server t port)

*)

(*
let uri port path =
  Uri.of_string (Printf.sprintf "http://127.0.0.1:%d/%s" port path)

let key k =
  Printf.sprintf "key/%s" (Memory.Types.string_of_key k)

let json_of_result fn result =
  match_lwt result with
  | None           -> failwith "json_of_body"
  | Some (_, body) ->
    lwt str = Body.string_of_body body in
    let res = fn (Memory.J.json_of_string str) in
    return res

let keys port =
  let result = Client.get (uri port "key") in
  json_of_result Memory.J.keys_of_json result

let tags port =
  let result = Client.get (uri port "tags") in
  json_of_result Memory.J.tags_of_json result

(* XXX: dummy implementation *)
let discover src dst =
  Lwt_unix.run (
    lwt keys = keys src in
    lwt tags = tags src in
    let args = Memory.J.string_of_json (Memory.J.json_of_discover (keys,tags)) in
    let body = Body.body_of_string args in
    let result = Client.post ?body (uri dst "action/discover") in
    lwt keys = json_of_result Memory.J.keys_of_json result in
    List.iter (fun k -> Printf.printf "%s\n" (Memory.Types.string_of_key k)) keys;
    return ()
  )

(* XXX: dummy implementation *)
let pull src dst =
  Lwt_unix.run (
    Client.get (uri src "key") >>= function
      | None           -> failwith "pull"
      | Some (_, body) ->
        Client.post ?body (uri dst "action/pull") >>= function
          | None           -> failwith "pull"
          | Some (_, body) ->
            let _ = Client.post ?body (uri src "action/push") in
            return ()
  )

(* XXX: dummy implementation *)
let push src dst =
  pull dst src

let add dst values =
  Lwt_unix.run (
    List.iter (fun value ->
        let args = Memory.J.string_of_json (Memory.J.json_of_string value) in
        let body = Body.body_of_string args in
        let _ = Client.post ?body (uri dst "action/add") in
        ()
      ) values;
    return ()
  )

let take _ =
  failwith "TODO"

let peek _ =
  failwith "TODO"

let watch _ =
  failwith "TODO"

let clone _ =
  failwith "TODO"

let dump dst =
  Lwt_unix.run (
    Client.get (uri dst "dump/queue") >>= function
      | None           ->
        Printf.printf "<empty>";
        return ()
      | Some (_, body) ->
        lwt json = Body.string_of_body body in
        let keys = Memory.J.keys_of_json (Memory.J.json_of_string json) in
        Lwt_list.iter_s (fun k ->
            Client.get (uri dst (key k)) >>= function
            | None           -> failwith "dump"
            | Some (_, body) ->
              lwt str = Body.string_of_body body in
              Printf.printf "%s\n" str;
              return ()
          ) keys
  )
*)
