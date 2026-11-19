(*
 * Copyright (c) 2018-2022 Tarides <contact@tarides.com>
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

open Lwt.Syntax
open Irmin_client_unix
open Util
module Info = Info (Client.Info)

let info = Info.v

let () =
  let style_renderer = `None in
  Fmt_tty.setup_std_outputs ~style_renderer ();
  Logs.set_level (Some Logs.Error);
  Logs.set_reporter (Logs_fmt.reporter ())

module type R = sig
  val uri : Uri.t
  val kind : string
end

module Make (R : R) = struct
  let config = Irmin_client_unix.config R.uri
  let client = Lwt_main.run (Client.Repo.v config)
  let clean ~config:_ = Client.Branch.remove client "main"

  module X = Irmin_mem.KV.Make (Irmin.Contents.String)
  module Store = Irmin_client_unix.Make (X)

  let suite =
    Irmin_test.Suite.create_generic_key ~name:R.kind
      ~store:(module Store)
      ~config ~clean ()
end

let unix_server = run_server `Unix_domain
let tcp_server = run_server `Tcp
let websocket_server = run_server `Websocket

module Unix_socket = Make (struct
  let kind, _, uri = unix_server
end)

module Tcp_socket = Make (struct
  let kind, _, uri = tcp_server
end)

module Websocket = Make (struct
  let kind, _, uri = websocket_server
end)

let _, _, uri = unix_server
let config = Irmin_client_unix.config uri
let client = Lwt_main.run (Client.Repo.v config)
let client () = Client.dup client

let error =
  Alcotest.testable (Fmt.using Error.to_string Fmt.string) (fun a b ->
      Error.to_string a = Error.to_string b)

let ty t =
  Alcotest.testable
    (Fmt.using (Irmin.Type.to_string t) Fmt.string)
    (fun a b -> Irmin.Type.(unstage (equal t)) a b)

let ping () =
  let open Client in
  let* client = client () in
  Logs.debug (fun l -> l "BEFORE PING");
  let+ r = ping client in
  Logs.debug (fun l -> l "AFTER PING");
  Alcotest.(check (result unit error)) "ping" (Ok ()) r

(* The dashboard sends the paths of the store as JSON strings, and reads
   them back from percent-encoded URLs, whatever they contain. *)
let dashboard_paths () =
  let entry_t =
    Irmin.Type.(
      record "entry" (fun path kind hash -> (path, kind, hash))
      |+ field "path" string (fun (path, _, _) -> path)
      |+ field "kind" string (fun (_, kind, _) -> kind)
      |+ field "hash" string (fun (_, _, hash) -> hash)
      |> sealr)
  in
  let contents_t =
    Irmin.Type.(
      record "contents" (fun contents hash -> (contents, hash))
      |+ field "contents" string fst
      |+ field "hash" string snd
      |> sealr)
  in
  (* [%41] must not be read as [A] *)
  let step = {|"<img src=x onerror=alert(1)>\ #?%41|} in
  (* Not valid UTF-8, so the paths below it are sent in base64 *)
  let binary = step ^ "\xff" in
  let path steps = "/" ^ String.concat "/" steps in
  let request meth steps =
    let encoded = path (List.map (Uri.pct_encode ~component:`Path) steps) in
    let uri =
      Uri.of_string
        (Fmt.str "http://127.0.0.1:%d%s?branch=dashboard" dashboard_port encoded)
    in
    let* response, body = Cohttp_lwt_unix.Client.call meth uri in
    let+ body = Cohttp_lwt.Body.to_string body in
    let headers = Cohttp.Response.headers response in
    (Cohttp.Header.get headers "content-type", body)
  in
  let decode t body =
    match Irmin.Type.of_json_string t body with
    | Ok x -> x
    | Error (`Msg e) -> Alcotest.failf "invalid JSON %S: %s" body e
  in
  let list steps =
    let+ content_type, body = request `POST steps in
    Alcotest.(check (option string))
      "content type" (Some "application/json") content_type;
    decode Irmin.Type.(list entry_t) body
  in
  let paths = List.map (fun (path, _, _) -> path) in
  let* repo = client () in
  let* t = Client.of_branch repo "dashboard" in
  Lwt.finalize
    (fun () ->
      let* () = Client.set_exn t ~info:Client.Info.none [ step; binary ] "v" in
      let* entries = list [] in
      Alcotest.(check (list string)) "root" [ path [ step ] ] (paths entries);
      let* entries = list [ step ] in
      Alcotest.(check (list string))
        "node"
        [ path [ step; binary ] ]
        (paths entries);
      let* content_type, body = request `POST [ step; binary ] in
      Alcotest.(check (option string))
        "contents type" (Some "application/json") content_type;
      let contents, hash = decode contents_t body in
      Alcotest.(check string) "contents" "v" contents;
      (* The hash of the contents is written as in the list *)
      Alcotest.(check (list string))
        "hash"
        (List.map (fun (_, _, hash) -> hash) entries)
        [ hash ];
      let+ content_type, _ = request `GET [] in
      Alcotest.(check (option string))
        "page content type" (Some "text/html; charset=utf-8") content_type)
    (fun () -> Client.Branch.remove repo "dashboard")

let misc =
  [ ("ping", `Quick, ping); ("dashboard paths", `Quick, dashboard_paths) ]

let misc = [ ("misc", misc) ]

let () =
  let slow = Sys.getenv_opt "SLOW" |> Option.is_some in
  let only = Sys.getenv_opt "ONLY" in
  let tests =
    match only with
    | Some "ws" -> [ (`Quick, Websocket.suite) ]
    | Some "tcp" -> [ (`Quick, Tcp_socket.suite) ]
    | Some "unix" -> [ (`Quick, Unix_socket.suite) ]
    | Some s -> failwith ("Invalid selection: " ^ s)
    | None ->
        [
          (`Quick, Unix_socket.suite);
          (`Quick, Tcp_socket.suite);
          (`Quick, Websocket.suite);
        ]
  in
  Lwt_main.run
    (Irmin_test.Store.run "irmin-server" ~sleep:Lwt_unix.sleep ~slow ~misc tests)
