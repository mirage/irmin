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

open Irmin_server
module Error = Error

let date = Jv.get Jv.global "Date"

let get_time () =
  let d = Jv.new' date [||] in
  let t = (Jv.call d "getTime" [||] |> Jv.to_float) /. 1000. in
  t

module Info (I : Irmin.Info.S) = struct
  include I

  let init = v

  let v ?author fmt =
    Fmt.kstr
      (fun message () ->
        let date = Int64.of_float @@ get_time () in
        init ?author ~message date)
      fmt
end

(* Buff is trying to be a single buffer that the Websocket shim can read
   handshakes and requests from and write responses too, whilst the internal
   irmin-server logic can write requests to it and read the responses from it. *)
module Buff = struct
  open Lwt.Infix

  type t = {
    mutable buf : Bigstringaf.t;
    mutable off : int;
    mutable len : int;
    waiters : unit Lwt.u Lwt_dllist.t;
  }

  let of_bigstring ~off ~len buf =
    assert (off >= 0);
    assert (Bigstringaf.length buf >= len - off);
    { buf; off; len; waiters = Lwt_dllist.create () }

  let create len = of_bigstring ~off:0 ~len:0 (Bigstringaf.create len)
  let writable_space t = Bigstringaf.length t.buf - t.len
  let trailing_space t = Bigstringaf.length t.buf - (t.off + t.len)

  let compress t =
    Bigstringaf.unsafe_blit t.buf ~src_off:t.off t.buf ~dst_off:0
      ~len:(t.len - t.off);
    t.len <- t.len - t.off;
    t.off <- 0

  let grow t to_copy =
    let old_len = Bigstringaf.length t.buf in
    let new_len = ref old_len in
    let space = writable_space t in
    while space + !new_len - old_len < to_copy do
      new_len := 3 * !new_len / 2
    done;
    let new_buf = Bigstringaf.create !new_len in
    Bigstringaf.unsafe_blit t.buf ~src_off:t.off new_buf ~dst_off:0
      ~len:(t.len - t.off);
    t.buf <- new_buf;
    t.len <- t.len - t.off;
    t.off <- 0

  let ensure t to_copy =
    if trailing_space t < to_copy then
      if writable_space t >= to_copy then compress t else grow t to_copy

  let write_pos t = t.len
  let unread_data t = t.len - t.off

  let wakeup t =
    if Lwt_dllist.is_empty t.waiters then ()
    else Lwt.wakeup_later (Lwt_dllist.take_l t.waiters) ()

  let add_string t str =
    let off = 0 and len = String.length str in
    ensure t len;
    Bigstringaf.unsafe_blit_from_string str ~src_off:off t.buf
      ~dst_off:(write_pos t) ~len;
    t.len <- t.len + len;
    wakeup t

  let add_int64_be t i =
    ensure t 8;
    Bigstringaf.set_int64_be t.buf (write_pos t) i;
    t.len <- t.len + 8;
    wakeup t

  let add_char t i =
    ensure t 1;
    Bigstringaf.set t.buf (write_pos t) i;
    t.len <- t.len + 1;
    wakeup t

  let read_int64_be t =
    let get_int64 t =
      let i = Bigstringaf.get_int64_be t.buf t.off in
      t.off <- t.off + 8;
      Lwt.return i
    in
    if unread_data t < 8 || not (Lwt_dllist.is_empty t.waiters) then
      let p, r = Lwt.wait () in
      let _node = Lwt_dllist.add_r r t.waiters in
      p >>= fun () -> get_int64 t
    else get_int64 t

  let read_char t =
    let get_char t =
      let i = Bigstringaf.get t.buf t.off in
      t.off <- t.off + 1;
      Lwt.return i
    in
    if unread_data t < 1 || not (Lwt_dllist.is_empty t.waiters) then
      let p, r = Lwt.wait () in
      let _node = Lwt_dllist.add_r r t.waiters in
      p >>= fun () -> get_char t
    else get_char t

  let read_into_exactly ~off ~len ~buf:t bs =
    let read t =
      Bigstringaf.blit_to_bytes t.buf ~src_off:t.off bs ~dst_off:off ~len;
      t.off <- t.off + len;
      Lwt.return ()
    in
    if unread_data t < len || not (Lwt_dllist.is_empty t.waiters) then
      let p, r = Lwt.wait () in
      let _node = Lwt_dllist.add_r r t.waiters in
      p >>= fun () -> read t
    else read t
end

module IO = struct
  type flow = unit
  type channel = { closed : Lwt_switch.t; buff : Buff.t }
  type ctx = unit

  let default_ctx = Lazy.from_val ()

  type ic = channel
  type oc = channel

  exception Timeout

  let is_closed { closed; _ } = not (Lwt_switch.is_on closed)
  let write_int64_be { buff; _ } i = Lwt.return @@ Buff.add_int64_be buff i
  let read_int64_be { buff; _ } = Buff.read_int64_be buff
  let write { buff; _ } i = Lwt.return @@ Buff.add_string buff i

  let read_into_exactly { buff; _ } bs off len =
    Buff.read_into_exactly ~off ~len ~buf:buff bs

  let write_char { buff; _ } c = Lwt.return @@ Buff.add_char buff c
  let read_char { buff; _ } = Buff.read_char buff

  let with_timeout d f =
    let open Lwt.Infix in
    Lwt.pick
      [ (Js_of_ocaml_lwt.Lwt_js.sleep d >>= fun () -> Lwt.fail Timeout); f () ]

  let time = get_time
  let flush _ = Lwt.return ()

  (* The websocket protocol reads fully formed protocol packets off of
     one end of a pipe given to irmin-server and converts the
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
      read_int64_be ic >>= fun b_length ->
      let length = Int64.to_int b_length in
      read_exactly ~length ic >|= fun data ->
      let buf = Buffer.create (8 + length) in
      Buffer.add_int64_be buf b_length;
      Buffer.add_string buf data;
      Buffer.contents buf

    let read_request ic =
      read_char ic >>= fun cmd_length ->
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

  let websocket_to_flow ws =
    let open Lwt.Infix in
    let open Brr in
    let open Brr_io in
    let fill_ic channel msg =
      let msg =
        Ev.as_type msg
        |> Message.Ev.data
        |> Tarray.Buffer.of_jv
        |> Tarray.of_buffer Tarray.Uint8
      in
      let msg = Tarray.to_string msg in
      Logs.debug (fun f -> f "<<< Client received frame");
      Lwt.async (fun () -> write channel msg)
    in
    let rec send_oc handshake channel ws =
      (if handshake then Websocket_protocol.read_handshake channel
       else Websocket_protocol.read_request channel)
      >>= fun content ->
      Logs.debug (fun f -> f ">>> Client sent frame");
      let len = String.length content in
      let content = Bigstringaf.of_string ~off:0 ~len content in
      let content =
        Js_of_ocaml.Typed_array.Bigstring.to_arrayBuffer content
        |> Jv.repr
        |> Tarray.Buffer.of_jv
      in
      Websocket.send_array_buffer ws content;
      send_oc false channel ws
    in
    let c1_switch = Lwt_switch.create () in
    Lwt_switch.add_hook (Some c1_switch) (fun () ->
        Lwt.return @@ Websocket.close ws);
    let c2_switch = Lwt_switch.create () in
    Lwt_switch.add_hook (Some c2_switch) (fun () ->
        Lwt.return @@ Websocket.close ws);
    let c1 = { closed = c1_switch; buff = Buff.create 4096 } in
    let c2 = { closed = c1_switch; buff = Buff.create 4096 } in
    let _ev =
      Brr.Ev.listen Message.Ev.message (fill_ic c1) (Websocket.as_target ws)
    in
    (* TODO: unlisten ? *)
    Lwt.async (fun () -> send_oc true c2 ws);
    (c1, c2)

  let connect ~sw:_ ~ctx:_ (client : Irmin_client.addr) =
    let open Lwt.Infix in
    match client with
    | `Ws (None, s) ->
        let open Brr_io in
        let ws = Websocket.create @@ Jstr.v s in
        let () =
          Websocket.set_binary_type ws Websocket.Binary_type.arraybuffer
        in
        let p, r = Lwt.wait () in
        let _ev =
          Brr.Ev.listen Brr.Ev.open'
            (fun _ -> Lwt.wakeup_later r ())
            (Websocket.as_target ws)
        in
        let _ev =
          Brr.Ev.listen Brr.Ev.error
            (fun _err ->
              Lwt.wakeup_later_exn r (Invalid_argument "Websocket Failure"))
            (Websocket.as_target ws)
        in
        (* TODO: unlisten ? *)
        if Websocket.ready_state ws = Websocket.Ready_state.open' then
          Lwt.wakeup_later r ();
        p >|= fun () -> websocket_to_flow ws
    | `Ws _ | `TLS _ | `TCP _ | `Unix_domain_socket _ ->
        failwith "Unsupported Websocket Protocol"

  let close (ic, oc) =
    let open Lwt.Infix in
    Lwt_switch.turn_off ic.closed >>= fun () -> Lwt_switch.turn_off oc.closed
end

let normalize_uri ?hostname uri =
  let addr = Uri.host_with_default ~default:"127.0.0.1" uri in
  (uri, Option.value ~default:addr hostname)

let config ?tls ?hostname uri =
  let uri, addr = normalize_uri ?hostname uri in
  Irmin_client.config ?tls ~hostname:(Option.value ~default:addr hostname) uri

module Make_codec (Codec : Conn.Codec.S) (Store : Irmin.Generic_key.S) = struct
  include Irmin_client.Make_codec (IO) (Codec) (Store)

  let connect ~sw ?tls ?hostname uri =
    let uri, hostname = normalize_uri ?hostname uri in
    connect ~sw ?tls ~hostname uri
end

module Make (Store : Irmin.Generic_key.S) = struct
  include Make_codec (Conn.Codec.Bin) (Store)
end

module Make_json (Store : Irmin.Generic_key.S) = struct
  include Make_codec (Conn.Codec.Json) (Store)
end
