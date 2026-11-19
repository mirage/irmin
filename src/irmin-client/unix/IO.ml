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
open Import

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
  let input_ic, input_oc = Lwt_io.pipe () in
  let output_ic, output_oc = Lwt_io.pipe () in
  (* An exception must not leave the two loops below: it would stop the
     program, as nobody waits for them. *)
  let stopped loop = function
    | End_of_file | Lwt.Canceled -> ()
    | Websocket.Protocol_error _ as exn ->
        [%log.warn "Websocket: %s stopped on %a" loop Fmt.exn exn]
    | exn -> [%log.info "Websocket: %s stopped on %a" loop Fmt.exn exn]
  in
  (* [ending what f] runs [f ()], which ends the connection, and only logs
     its failure *)
  let ending what f =
    Lwt.catch f (fun exn ->
        [%log.info "Websocket: %s: %a" what Fmt.exn exn];
        Lwt.return_unit)
  in
  (* Set when the connection is over: no read may wait any more *)
  let closing = ref false in
  let read_frame () =
    let frame = Websocket_lwt_unix.read client in
    if !closing then Lwt.cancel frame;
    frame
  in
  (* The frames of the server are the input of the client. [fragment] tells
     whether a binary message is not complete yet. *)
  let rec receive ~fragment () =
    read_frame () >>= fun frame ->
    match frame.opcode with
    | Binary -> forward frame
    | Continuation when fragment -> forward frame
    (* A text frame inside a binary message breaks the protocol: the client
       then fails to decode the part of the message it was given *)
    | Text | Continuation -> receive ~fragment:false ()
    | Close -> Lwt.fail End_of_file
    | _ -> receive ~fragment ()
  and forward (frame : Websocket.Frame.t) =
    [%log.debug "<<< Client received frame"];
    Lwt_io.write input_oc frame.content >>= receive ~fragment:(not frame.final)
  in
  (* Each message written by the client is a frame for the server *)
  let rec send read =
    read output_ic >>= fun content ->
    [%log.debug ">>> Client sent frame"];
    Websocket_lwt_unix.write client
      (Websocket.Frame.create ~opcode:Binary ~content ())
    >>= fun () -> send Websocket_protocol.read_request
  in
  (* The input channel of the websocket is only closed by a read which
     fails: the frames left are read until then *)
  let rec drain () = read_frame () >>= fun _ -> drain () in
  let receiver =
    Lwt.finalize
      (fun () ->
        Lwt.catch (receive ~fragment:false) (fun exn ->
            stopped "receiver" exn;
            (* The client reads what is left, then the end of its input *)
            ending "closing the input" (fun () -> Lwt_io.close input_oc)))
      (fun () ->
        (* A read always ends the drain with an exception *)
        Lwt.catch drain (fun exn ->
            [%log.debug "Websocket: end of the input: %a" Fmt.exn exn];
            Lwt.return_unit))
  in
  Lwt.async (fun () ->
      Lwt.catch
        (fun () -> send Websocket_protocol.read_handshake)
        (fun exn ->
          stopped "sender" exn;
          Lwt.return_unit)
      >>= fun () ->
      (* The client is closed, or nothing can be sent any more: the
         connection is over *)
      ending "closing the output" (fun () -> Lwt_io.abort output_ic)
      >>= fun () ->
      (* A cancelled read closes the input of the websocket, and
         [close_transport] its output: the socket is then closed *)
      closing := true;
      Lwt.cancel receiver;
      ending "closing the connection" (fun () ->
          Websocket_lwt_unix.close_transport client));
  (input_ic, output_oc)

let connect ~ctx (client : Irmin_client.addr) =
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
