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


let uri port path =
  Uri.of_string (Printf.sprintf "http://127.0.0.1:%d/%s" port path)

let commit port =
  Lwt_unix.run (
    lwt _ = Client.get (uri port "action/snapshot") in
    return ()
  )

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

let add _ =
  failwith "TODO"

let take _ =
  failwith "TODO"

let peek _ =
  failwith "TODO"

let watch _ =
  failwith "TODO"

let clone _ =
  failwith "TODO"

let dump _ =
  failwith "TODO"
