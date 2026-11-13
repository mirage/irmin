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

open Lwt.Infix

let () = Mirage_crypto_rng_unix.use_default ()

module Store = Irmin_mem.KV.Make (Irmin.Contents.String)
module Client = Irmin_client_unix.Make (Store)
module Server = Irmin_server_unix.Make (Store)

let test name f client _switch () =
  Logs.debug (fun l -> l "Running: %s" name);
  f client

(* A port which is free for now. *)
let free_port () =
  let socket = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Fun.protect
    ~finally:(fun () -> Unix.close socket)
    (fun () ->
      Unix.bind socket (Unix.ADDR_INET (Unix.inet_addr_loopback, 0));
      match Unix.getsockname socket with
      | Unix.ADDR_INET (_, port) -> port
      | Unix.ADDR_UNIX _ -> failwith "free_port: not an internet address")

(* Wait until a server accepts the connections. *)
let wait_for_server ~clock uri =
  let addr =
    match (Uri.scheme uri, Uri.port uri) with
    | Some "unix", _ -> Unix.ADDR_UNIX (Uri.path uri)
    | _, Some port -> Unix.ADDR_INET (Unix.inet_addr_loopback, port)
    | _, None -> Fmt.failwith "No port in %a" Uri.pp uri
  in
  let connect () =
    let socket =
      Unix.socket (Unix.domain_of_sockaddr addr) Unix.SOCK_STREAM 0
    in
    Fun.protect
      ~finally:(fun () -> Unix.close socket)
      (fun () ->
        match Unix.connect socket addr with
        | () -> true
        | exception Unix.Unix_error ((Unix.ECONNREFUSED | Unix.ENOENT), _, _) ->
            false)
  in
  let rec wait tries =
    if not (connect ()) then
      if tries = 0 then Fmt.failwith "No server on %a" Uri.pp uri
      else (
        Eio.Time.sleep clock 0.05;
        wait (tries - 1))
  in
  wait 400

let mem_config kind =
  let spec = Irmin.Backend.Conf.Spec.v kind in
  let key = Irmin.Backend.Conf.root spec in
  Irmin.Backend.Conf.singleton spec key kind

let dashboard_port = free_port ()

(* Run a server for as long as [sw] is on. *)
let start_server ~sw ~clock ?dashboard ?max_message_size kind uri =
  let stop, set_stop = Lwt.wait () in
  Eio.Switch.on_release sw (fun () -> Lwt.wakeup_later set_stop ());
  Eio.Fiber.fork_daemon ~sw (fun () ->
      let () = Irmin.Backend.Watch.set_listen_dir_hook Irmin_watcher.hook in
      let conf = mem_config kind in
      Lwt_eio.run_lwt (fun () ->
          Server.v ?dashboard ?max_message_size ~uri conf >>= Server.serve ~stop);
      `Stop_daemon);
  wait_for_server ~clock uri

let run_server ~sw ~clock s =
  let uri scheme =
    Uri.of_string (Fmt.str "%s://localhost:%d" scheme (free_port ()))
  in
  let kind, uri =
    match s with
    | `Websocket -> ("Websocket", uri "ws")
    | `Unix_domain ->
        let dir = Unix.getcwd () in
        let sock = Filename.concat dir "test.sock" in
        ("Unix_domain", Uri.of_string ("unix://" ^ sock))
    | `Tcp -> ("Tcp", uri "tcp")
  in
  let dashboard =
    match s with
    | `Unix_domain -> Some (`TCP (`Port dashboard_port))
    | `Tcp | `Websocket -> None
  in
  start_server ~sw ~clock ?dashboard kind uri;
  (kind, uri)
