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

type flow = Conduit_lwt_unix.flow
type ic = Conduit_lwt_unix.ic
type oc = Conduit_lwt_unix.oc
type ctx = Conduit_lwt_unix.ctx

exception Timeout = Lwt_unix.Timeout

let default_ctx = Conduit_lwt_unix.default_ctx
let is_closed (x : ic) = Lwt_io.is_closed x
let write_int64_be = Lwt_io.BE.write_int64
let read_int64_be = Lwt_io.BE.read_int64
let flush = Lwt_io.flush
let write = Lwt_io.write
let read_into_exactly = Lwt_io.read_into_exactly
let write_char = Lwt_io.write_char
let read_char = Lwt_io.read_char

(* The websocket protocol reads fully formed protocol packets off of
   one end of a pipe given to irmin-server-internal and converts the
   packet into a single websocket message. On the client this means
   being able to _read_ the client-constructed handshake and request
   messages. Note, we reconstruct the packet as a string so the server
   simply has to write the string it receives to a pipe. *)
module Websocket_protocol = struct
  open Lwt.Infix

  let read_exactly ~length ic =
    let buff = Bytes.create length in
    read_into_exactly ic buff 0 length >|= fun () -> Bytes.to_string buff

  let read_handshake ic =
    Lwt_io.BE.read_int64 ic >>= fun b_length ->
    let length = Int64.to_int b_length in
    read_exactly ~length ic >|= fun data ->
    let buf = Buffer.create (8 + length) in
    Buffer.add_int64_be buf b_length;
    Buffer.add_string buf data;
    Buffer.contents buf

  let read_request ic =
    Lwt_io.read_char ic >>= fun cmd_length ->
    let cl = int_of_char cmd_length in
    read_exactly ~length:cl ic >>= fun cmd ->
    read_int64_be ic >>= fun b_length ->
    let length = Int64.to_int b_length in
    read_exactly ~length ic >|= fun data ->
    let buf = Buffer.create (1 + cl + 8 + length) in
    Buffer.add_char buf cmd_length;
    Buffer.add_string buf cmd;
    Buffer.add_int64_be buf b_length;
    Buffer.add_string buf data;
    Buffer.contents buf
end

let websocket_to_flow client =
  let open Lwt.Infix in
  let rec fill_ic channel client =
    (* There's no way to test if the connected client is still alive
       so we catch the End_of_file exception and presume it means the
       connection is now over. *)
    Lwt.catch
      (fun () ->
        Websocket_lwt_unix.read client >>= fun frame ->
        Logs.debug (fun f -> f "<<< Client received frame");
        Lwt_io.write channel frame.content >>= fun () -> fill_ic channel client)
      (function End_of_file -> Lwt_io.close channel | exn -> Lwt.fail exn)
  in
  let rec send_oc handshake channel client =
    (if handshake then Websocket_protocol.read_handshake channel
     else Websocket_protocol.read_request channel)
    >>= fun content ->
    Logs.debug (fun f -> f ">>> Client sent frame");
    Lwt.catch
      (fun () ->
        Websocket_lwt_unix.write client
          (Websocket.Frame.create ~opcode:Binary ~content ())
        >>= fun () -> send_oc false channel client)
      (function End_of_file -> Lwt_io.close channel | exn -> Lwt.fail exn)
  in
  let input_ic, input_oc = Lwt_io.pipe () in
  let output_ic, output_oc = Lwt_io.pipe () in
  Lwt.async (fun () -> fill_ic input_oc client);
  Lwt.async (fun () -> send_oc true output_ic client);
  (input_ic, output_oc)

let connect ~sw:_ ~ctx (client : Irmin_client.addr) =
  let open Lwt.Infix in
  match client with
  | (`TLS _ | `TCP _ | `Unix_domain_socket _) as client ->
      Conduit_lwt_unix.connect ~ctx (client :> Conduit_lwt_unix.client)
      >|= fun (_, ic, oc) -> (ic, oc)
  | `Ws (Some (host, port), uri) ->
      Websocket_lwt_unix.connect ~ctx (`TCP (host, port)) (Uri.of_string uri)
      >|= fun ws -> websocket_to_flow ws
  | `Ws _ -> failwith "The Unix client requires a IP address and port number"

let close (c : ic * oc) = Conduit_lwt_server.close c
let with_timeout = Lwt_unix.with_timeout
let time = Unix.time
