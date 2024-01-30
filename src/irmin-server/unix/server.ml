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
open Lwt.Infix
open Irmin_server
include Server_intf

let html = [%blob "index.html"]

module Make (Codec : Conn.Codec.S) (Store : Irmin.Generic_key.S) = struct
  module Command = Command.Make (IO) (Codec) (Store)
  module Store = Store
  module Conn = Command.Conn

  type t = {
    ctx : Conduit_lwt_unix.ctx;
    uri : Uri.t;
    dashboard : Conduit_lwt_unix.server option;
    server : Conduit_lwt_unix.server;
    config : Irmin.config;
    repo : Store.Repo.t;
    info : Command.Server_info.t;
  }

  module Client_set = Set.Make (struct
    type t = Command.context

    let compare a b =
      let conn = a.Command.conn.ic in
      let conn' = b.Command.conn.ic in
      compare conn conn'
  end)

  let readonly conf =
    Irmin.Backend.Conf.add conf Irmin_pack.Conf.Key.readonly true

  let v ~sw ?tls_config ?dashboard ~uri config =
    let scheme = Uri.scheme uri |> Option.value ~default:"tcp" in
    let* ctx, server =
      match String.lowercase_ascii scheme with
      | "unix" ->
          let file = Uri.path uri in
          let+ () =
            Lwt.catch
              (fun () -> Lwt_unix.unlink file)
              (fun _ -> Lwt.return_unit)
          in
          ( Lazy.force Conduit_lwt_unix.default_ctx,
            `Unix_domain_socket (`File file) )
      | "tcp" | "ws" | "wss" -> (
          let addr = Uri.host_with_default ~default:"127.0.0.1" uri in
          let ip = Unix.gethostbyname addr in
          let addr = ip.h_addr_list.(0) |> Unix.string_of_inet_addr in
          let+ ctx = Conduit_lwt_unix.init ~src:addr () in
          let port = Uri.port uri |> Option.value ~default:9181 in
          match tls_config with
          | None -> (ctx, `TCP (`Port port))
          | Some (`Cert_file crt, `Key_file key) ->
              ( ctx,
                `TLS
                  ( `Crt_file_path crt,
                    `Key_file_path key,
                    `No_password,
                    `Port port ) ))
      | x -> invalid_arg ("Unknown server scheme: " ^ x)
    in
    let+ repo = Lwt_eio.run_eio @@ fun () -> Store.Repo.v ~sw config in
    let start_time = Unix.time () in
    let info = Command.Server_info.{ start_time } in
    { ctx; uri; server; dashboard; config; repo; info }

  let commands = Hashtbl.create (List.length Command.commands)
  let () = Hashtbl.replace_seq commands (List.to_seq Command.commands)
  let invalid_arguments a = Error.unwrap "Invalid arguments" a [@@inline]

  let[@tailrec] rec loop repo conn client info : unit Lwt.t =
    if Conn.is_closed conn then
      let* () =
        Lwt_eio.run_eio @@ fun () ->
        match client.Command.watch with Some w -> Store.unwatch w | None -> ()
      in
      let* () =
        Lwt_eio.run_eio @@ fun () ->
        match client.Command.branch_watch with
        | Some w ->
            let b = Store.Backend.Repo.branch_t client.repo in
            Store.Backend.Branch.unwatch b w
        | None -> ()
      in
      Lwt.return_unit
    else
      Lwt.catch
        (fun () ->
          Logs.debug (fun l -> l "Receiving next command");
          (* Get request header (command and number of arguments) *)
          let* Conn.Request.{ command } = Conn.Request.read_header conn in
          (* Get command *)
          match Hashtbl.find_opt commands command with
          | None ->
              if String.length command = 0 then Lwt.return_unit
              else
                let () = Logs.err (fun l -> l "Unknown command: %s" command) in
                Conn.err conn ("unknown command: " ^ command)
          | Some (module Cmd : Command.CMD) ->
              let* req = Conn.read conn Cmd.req_t >|= invalid_arguments in
              Logs.debug (fun l -> l "Command: %s" Cmd.name);
              let* res = Cmd.run conn client info req in
              Conn.Return.finish res)
        (function
          | Error.Error s ->
              (* Recover *)
              Logs.debug (fun l -> l "Error response: %s" s);
              let* () = Conn.err conn s in
              Lwt_unix.sleep 0.01
          | End_of_file ->
              (* Client has disconnected *)
              let* () = Lwt_io.close conn.ic in
              Lwt.return_unit
          | exn ->
              if Conn.is_closed conn then Lwt.return_unit
              else
                (* Unhandled exception *)
                let s = Printexc.to_string exn in
                Logs.err (fun l ->
                    l "Exception: %s\n%s" s (Printexc.get_backtrace ()));
                let* () = Conn.err conn s in
                Lwt_unix.sleep 0.01)
      >>= fun () -> loop repo conn client info

  let callback { repo; info; config; _ } ic oc =
    (* Handshake check *)
    let conn = Conn.v ic oc in
    let* check =
      Lwt.catch
        (fun () -> Conn.Handshake.V1.check (module Store) conn)
        (fun _ -> Lwt.return_false)
    in
    if not check then
      (* Hanshake failed *)
      let () =
        Logs.info (fun l -> l "Client closed because of invalid handshake")
      in
      Lwt_io.close ic
    else
      (* Handshake ok *)
      let branch = Store.Branch.main in
      let* store = Lwt_eio.run_eio @@ fun () -> Store.of_branch repo branch in
      let trees = Hashtbl.create 8 in
      let client =
        Command.
          {
            conn;
            repo;
            branch;
            store;
            trees;
            watch = None;
            branch_watch = None;
            config;
          }
      in
      loop repo conn client info

  (* The websocket protocol reads fully formed protocol packets off of
     one end of a pipe given to irmin-server-internal and converts the
     packet into a single websocket message. On the server this means
     being able to _read_ the server-constructed handshake and response
     messages. Note, we reconstruct the packet as a string so the client
     simply has to write the string it receives to a pipe. *)
  module Websocket_protocol = struct
    open Lwt.Infix

    let read_exactly ~length ic =
      let buff = Bytes.create length in
      Lwt_io.read_into_exactly ic buff 0 length >|= fun () ->
      Bytes.to_string buff

    let read_handshake ic =
      Lwt_io.BE.read_int64 ic >>= fun b_length ->
      let length = Int64.to_int b_length in
      read_exactly ~length ic >|= fun data ->
      let buf = Buffer.create (8 + length) in
      Buffer.add_int64_be buf b_length;
      Buffer.add_string buf data;
      Buffer.contents buf

    let read_response ic =
      Lwt_io.read_char ic >>= fun status ->
      Lwt_io.BE.read_int64 ic >>= fun b_length ->
      let length = Int64.to_int b_length in
      read_exactly ~length ic >|= fun data ->
      let buf = Buffer.create (1 + 8 + length) in
      Buffer.add_char buf status;
      Buffer.add_int64_be buf b_length;
      Buffer.add_string buf data;
      Buffer.contents buf
  end

  let websocket_handler server client =
    let rec fill_ic channel other_channel client =
      if Lwt_io.is_closed other_channel then Lwt_io.close channel
      else
        Lwt.catch
          (fun () ->
            let* frame = Websocket_lwt_unix.Connected_client.recv client in
            if frame.opcode <> Binary then fill_ic channel other_channel client
            else
              let () = Logs.debug (fun f -> f "<<< Server received frame") in
              Lwt_io.write channel frame.content >>= fun () ->
              fill_ic channel other_channel client)
          (function
            | End_of_file ->
                (* The websocket has been closed is the assumption here *)
                Lwt_io.close channel >>= fun () -> Lwt_io.close other_channel
            | exn -> Lwt.fail exn)
    in
    let rec send_oc handshake channel other_channel client =
      if Lwt_io.is_closed other_channel then Lwt_io.close channel
      else
        (if handshake then Websocket_protocol.read_handshake channel
         else Websocket_protocol.read_response channel)
        >>= fun content ->
        Logs.debug (fun f -> f ">>> Server sent frame");
        Lwt.catch
          (fun () ->
            Websocket_lwt_unix.Connected_client.send client
              (Websocket.Frame.create ~opcode:Binary ~content ())
            >>= fun () -> send_oc false channel other_channel client)
          (function
            | End_of_file ->
                Lwt_io.close channel >>= fun () -> Lwt_io.close other_channel
            | exn -> Lwt.fail exn)
    in
    let input_ic, input_oc = Lwt_io.pipe () in
    let output_ic, output_oc = Lwt_io.pipe () in
    Lwt.async (fun () -> fill_ic input_oc input_ic client);
    Lwt.async (fun () -> send_oc true output_ic output_oc client);
    callback server input_ic output_oc

  let on_exn x = Logs.err (fun l -> l "EXCEPTION: %s" (Printexc.to_string x))

  let dashboard t mode =
    let list store prefix =
      let keys = Store.list store prefix in
      List.filter_map
        (fun (path, tree) ->
          let path = Store.Path.rcons prefix path in
          let kind = Store.Tree.kind tree Store.Path.empty in
          match kind with
          | Some `Contents -> Some (path, "contents", Store.Tree.hash tree)
          | Some `Node -> Some (path, "node", Store.Tree.hash tree)
          | None -> None)
        keys
    in
    let data_callback prefix branch =
      let* store =
        Lwt_eio.run_eio @@ fun () ->
        match branch with
        | `Hash commit -> (
            let commit = Store.Commit.of_hash t.repo commit in
            match commit with
            | Some commit -> Store.of_commit commit
            | None -> failwith "Invalid commit")
        | `Branch branch -> Store.of_branch t.repo branch
      in
      let* is_contents =
        Lwt_eio.run_eio @@ fun () ->
        match Store.kind store prefix with Some `Contents -> true | _ -> false
      in
      let res = Cohttp_lwt_unix.Response.make ~status:`OK () in
      if is_contents then
        let* contents = Lwt_eio.run_eio @@ fun () -> Store.get store prefix in
        let contents' = Irmin.Type.to_json_string Store.contents_t contents in
        let body =
          Printf.sprintf {|{"contents": %s, "hash": %s }|} contents'
            (Irmin.Type.to_json_string Store.hash_t
               (Store.Contents.hash contents))
        in
        let body = Cohttp_lwt.Body.of_string body in
        Lwt.return (res, body)
      else
        let* keys = Lwt_eio.run_eio @@ fun () -> list store prefix in
        let keys =
          List.map
            (fun (path, kind, hash) ->
              Format.sprintf {|{"path": "%s", "kind": "%s", "hash": "%s"}|}
                (Irmin.Type.to_string Store.path_t path)
                kind
                (Irmin.Type.to_string Store.hash_t hash))
            keys
        in
        let keys = String.concat "," keys in
        let body = Cohttp_lwt.Body.of_string (Printf.sprintf "[%s]" keys) in
        Lwt.return (res, body)
    in
    let callback _conn req body =
      let* () = Cohttp_lwt.Body.drain_body body in
      let uri = Cohttp_lwt_unix.Request.uri req in
      let path = Uri.path uri in
      let branch_name =
        Uri.get_query_param uri "branch" |> Option.value ~default:"main"
      in
      let branch =
        match Irmin.Type.of_string Store.hash_t branch_name with
        | Ok x -> `Hash x
        | Error _ ->
            `Branch
              (Result.get_ok @@ Irmin.Type.of_string Store.branch_t branch_name)
      in
      let prefix = Irmin.Type.of_string Store.path_t path |> Result.get_ok in
      let meth = Cohttp_lwt_unix.Request.meth req in
      match meth with
      | `POST -> data_callback prefix branch
      | `GET ->
          let res = Cohttp_lwt_unix.Response.make () in
          let body = Cohttp_lwt.Body.of_string html in
          Lwt.return (res, body)
      | _ ->
          let res = Cohttp_lwt_unix.Response.make ~status:`Not_found () in
          let body = Cohttp_lwt.Body.of_string "Not found" in
          Lwt.return (res, body)
    in
    let server = Cohttp_lwt_unix.Server.make ~callback () in
    Cohttp_lwt_unix.Server.create ~mode server

  let serve ?stop t =
    let unlink () =
      match Uri.scheme t.uri with
      | Some "unix" -> Unix.unlink (Uri.path t.uri)
      | _ -> ()
    in
    let _ =
      Lwt_unix.on_signal Sys.sigint (fun _ ->
          unlink ();
          exit 0)
    in
    let _ =
      Lwt_unix.on_signal Sys.sigterm (fun _ ->
          unlink ();
          exit 0)
    in
    let dashboard =
      match t.dashboard with
      | Some server -> dashboard t server
      | None -> Lwt.return_unit
    in
    let server =
      match Uri.scheme t.uri with
      | Some "ws" | Some "wss" ->
          Websocket_lwt_unix.establish_standard_server ~ctx:t.ctx ~mode:t.server
            ~on_exn
            ~check_request:(fun _ -> true)
            (websocket_handler t)
      | _ ->
          Conduit_lwt_unix.serve ?stop ~ctx:t.ctx ~on_exn ~mode:t.server
            (fun _ ic oc -> callback t ic oc)
    in
    let* () = Lwt.join [ server; dashboard ] in
    Lwt.wrap (fun () -> unlink ())
end
