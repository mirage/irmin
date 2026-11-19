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

open Irmin_client_unix
open Util
module Info = Info (Client.Info)

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

  module X = Irmin_mem.KV.Make (Irmin.Contents.String)
  module Store = Irmin_client_unix.Make (X)

  let suite () =
    let client = Client.Repo.v config in
    let clean ~config:_ = Client.Branch.remove client "main" in
    Irmin_test.Suite.create_generic_key ~name:R.kind
      ~store:(module Store)
      ~config ~clean ()
end

let error =
  Alcotest.testable (Fmt.using Error.to_string Fmt.string) (fun a b ->
      Error.to_string a = Error.to_string b)

let ping client () =
  let open Client in
  let client = client () in
  Logs.debug (fun l -> l "BEFORE PING");
  let r = ping client in
  Logs.debug (fun l -> l "AFTER PING");
  Alcotest.(check (result unit error)) "ping" (Ok ()) r

(* The dashboard sends the paths of the store as JSON strings, and reads
   them back from percent-encoded URLs, whatever they contain. *)
let dashboard_paths client () =
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
        (Fmt.str "http://127.0.0.1:%d%s?branch=dashboard"
           (Lazy.force dashboard_port)
           encoded)
    in
    let response, body =
      Lwt_eio.run_lwt @@ fun () ->
      let open Lwt.Syntax in
      let* response, body = Cohttp_lwt_unix.Client.call meth uri in
      let+ body = Cohttp_lwt.Body.to_string body in
      (response, body)
    in
    let headers = Cohttp.Response.headers response in
    (Cohttp.Header.get headers "content-type", body)
  in
  let decode t body =
    match Irmin.Type.of_json_string t body with
    | Ok x -> x
    | Error (`Msg e) -> Alcotest.failf "invalid JSON %S: %s" body e
  in
  let list steps =
    let content_type, body = request `POST steps in
    Alcotest.(check (option string))
      "content type" (Some "application/json") content_type;
    decode Irmin.Type.(list entry_t) body
  in
  let paths = List.map (fun (path, _, _) -> path) in
  let repo = client () in
  let t = Client.of_branch repo "dashboard" in
  Fun.protect
    ~finally:(fun () -> Client.Branch.remove repo "dashboard")
    (fun () ->
      Client.set_exn t ~info:Client.Info.none [ step; binary ] "v";
      Alcotest.(check (list string)) "root" [ path [ step ] ] (paths (list []));
      let entries = list [ step ] in
      Alcotest.(check (list string))
        "node"
        [ path [ step; binary ] ]
        (paths entries);
      let content_type, body = request `POST [ step; binary ] in
      Alcotest.(check (option string))
        "contents type" (Some "application/json") content_type;
      let contents, hash = decode contents_t body in
      Alcotest.(check string) "contents" "v" contents;
      (* The hash of the contents is written as in the list *)
      Alcotest.(check (list string))
        "hash"
        (List.map (fun (_, _, hash) -> hash) entries)
        [ hash ];
      let content_type, _ = request `GET [] in
      Alcotest.(check (option string))
        "page content type" (Some "text/html; charset=utf-8") content_type)

let misc client =
  [
    ("ping", `Quick, ping client);
    ("dashboard paths", `Quick, dashboard_paths client);
  ]

let misc client = [ ("misc", misc client) ]

let unix_suite ~env () =
  Eio.Switch.run @@ fun sw ->
  let clock = Eio.Stdenv.clock env in
  let kind, uri = run_server ~sw ~clock `Unix_domain in
  let module Unix_socket = Make (struct
    let uri = uri
    let kind = kind
  end) in
  let tests = Unix_socket.suite () in
  let config = Irmin_client_unix.config uri in
  let client = Client.Repo.v config in
  let client () = Lwt_eio.run_lwt @@ fun () -> Client.dup client in
  Irmin_test.Store.run "irmin-server.unix" ~and_exit:false ~sleep:Eio_unix.sleep
    ~misc:(misc client)
    [ (`Quick, tests) ]

let tcp_suite ~env () =
  Eio.Switch.run @@ fun sw ->
  let clock = Eio.Stdenv.clock env in
  let module Tcp_socket = Make (struct
    let kind, uri = run_server ~sw ~clock `Tcp
  end) in
  let tests = Tcp_socket.suite () in
  Irmin_test.Store.run "irmin-server.tcp" ~and_exit:false ~sleep:Eio_unix.sleep
    ~misc:[]
    [ (`Quick, tests) ]

let websocket_suite ~env () =
  Eio.Switch.run @@ fun sw ->
  let clock = Eio.Stdenv.clock env in
  let module Websocket = Make (struct
    let kind, uri = run_server ~sw ~clock `Websocket
  end) in
  let tests = Websocket.suite () in
  Irmin_test.Store.run "irmin-server.ws" ~and_exit:false ~sleep:Eio_unix.sleep
    ~misc:[]
    [ (`Quick, tests) ]

let main ~env () =
  let suites =
    [ ("tcp", tcp_suite); ("unix", unix_suite); ("ws", websocket_suite) ]
  in
  match Sys.getenv_opt "ONLY" with
  | None -> List.iter (fun (_, suite) -> suite ~env ()) suites
  | Some suite_name -> (
      match List.assoc_opt suite_name suites with
      | Some suite -> suite ~env ()
      | None -> failwith ("Invalid selection: " ^ suite_name))

let () =
  Eio_main.run @@ fun env ->
  Lwt_eio.with_event_loop ~clock:env#clock @@ fun _ -> main ~env ()
