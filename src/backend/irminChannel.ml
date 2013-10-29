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

module type S = sig
  type t
  type channel
  val create: channel -> string -> t
  val name: t -> string
  val channel: t -> channel
  val read_string: t -> int -> string Lwt.t
  val write_string: t -> string -> unit Lwt.t
  val read_ba: t -> int -> Cstruct.buffer Lwt.t
  val write_ba: t -> Cstruct.buffer -> unit Lwt.t
  val read_buffer: t -> IrminBuffer.t Lwt.t
  val write_buffer: t -> IrminBuffer.t -> unit Lwt.t
  val read_contents_length: t -> int Lwt.t
  val write_contents_length: t -> int -> unit Lwt.t
  val read_unit: t -> unit Lwt.t
  val write_unit: t -> unit Lwt.t
  val close: t -> unit Lwt.t
end

open Lwt

type channel = Lwt_unix.file_descr

type t = {
  fd  : channel;
  name: string;
}

let debug fmt = IrminLog.debug "CHANNEL" fmt

let name t = t.name

let channel t = t.fd

let close t = Lwt_unix.close t.fd

let read_string t len =
  debug "read_string %s (%d)" t.name len;
  if len = 0 then return ""
  else begin
    let str = String.create len in
    let rec rread fd str ofs len =
      Lwt_unix.read fd str ofs len >>= fun n ->
      debug "|-- read_string (+%d)" n;
      if n = 0 then fail End_of_file
      else if n < len then rread fd str (ofs + n) (len - n)
      else return () in
    rread t.fd str 0 len >>= fun () ->
    debug "<-- read_string %S" str;
    return str
  end

let write_string t str =
  debug "write_string %s %S" t.name str;
  if str = "" then return_unit
  else begin
    let rec rwrite fd str ofs len =
      Lwt_unix.write fd str ofs len >>= fun n ->
      if n = 0 then fail End_of_file
      else if n < len then rwrite fd str (ofs + n) (len - n)
      else return () in
    rwrite t.fd str 0 (String.length str)
  end

let read_ba t len =
  debug "read_ba %s (%d)" t.name len;
  if len = 0 then return (IrminBuffer.create_ba 0)
  else begin
    let buf = IrminBuffer.create_ba len in
    let rec rread fd buf ofs len =
      debug "rread ofs=%d len=%d" ofs len;
      Lwt_bytes.read fd buf ofs len >>= fun n ->
      if n = 0 then fail End_of_file
      else if n < len then rread fd buf (ofs + n) (len - n)
      else return () in
    rread t.fd buf 0 len >>= fun () ->
    IrminBuffer.dump_ba ~msg:"<--" buf;
    return buf
  end

let write_ba t ba =
  debug "write_ba %s" t.name;
  IrminBuffer.dump_ba ba;
  let rec rwrite fd buf ofs len =
    debug " ... write_buf %d" len;
    Lwt_bytes.write fd buf ofs len >>= fun n ->
    if n = 0 then fail End_of_file
    else if n < len then rwrite fd buf (ofs + n) (len - n)
    else return () in
  rwrite t.fd ba 0 (Bigarray.Array1.dim ba)

(* XXX: int32! *)
let read_contents_length t =
  debug "read_contents_length %s" t.name;
  read_string t 4 >>= fun str ->
  let len = EndianString.BigEndian.get_int32 str 0 in
  debug "<-- read_contents_length = %ld" len;
  return (Int32.to_int len)

(* XXX: int32! *)
let write_contents_length t len =
  debug "write_contents_length %s %dl" t.name len;
  let str = String.create 4 in
  EndianString.BigEndian.set_int32 str 0 (Int32.of_int len);
  write_string t str >>= fun () ->
  debug "<-- write_contents_length";
  return ()

let read_buffer t =
  debug "read_buffer %s" t.name;
  read_contents_length t >>= fun len ->
  read_ba t len >>= fun ba ->
  return (IrminBuffer.of_ba ba)

let write_buffer t buf =
  debug "write_buffer %s" t.name;
  IrminBuffer.dump ~msg:"<--" buf;
  let ba = IrminBuffer.to_ba buf in
  let len = IrminBuffer.ba_length ba in
  write_contents_length t len >>= fun () ->
  write_ba t ba

let write_unit t =
  write_string t "\000"

let read_unit t =
  read_string t 1 >>= fun str ->
  assert (str = "\000");
  return ()

let create fd name = { fd; name }

let unix_socket_server ~limit file =
  debug "unix-socker-server %s" file;
  let fd = Lwt_unix.(socket PF_UNIX SOCK_STREAM 0) in
  Lwt_unix.bind fd (Lwt_unix.ADDR_UNIX file);
  Lwt_unix.listen fd limit;
  create fd file

let unix_socket_client file =
  debug "unix-socket-client %s" file;
  let fd = Lwt_unix.(socket PF_UNIX SOCK_STREAM 0) in
  Lwt_unix.connect fd (Lwt_unix.ADDR_UNIX file) >>= fun () ->
  return (create fd file)
