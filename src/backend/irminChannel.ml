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
  val name: t -> string
  val read_string: t -> int -> string Lwt.t
  val write_string: t -> string -> unit Lwt.t
  val read_ba: t -> int -> Cstruct.buffer Lwt.t
  val write_ba: t -> Cstruct.buffer -> unit Lwt.t
  val read_length: t -> int Lwt.t
  val write_length: t -> int -> unit Lwt.t
  val read_unit: t -> unit Lwt.t
  val write_unit: t -> unit Lwt.t
  val close: t -> unit Lwt.t
  val of_file: string -> t Lwt.t
  val file_exits: string -> bool Lwt.t
  val is_directory: string -> bool Lw.t
  val mkdir: string -> unit Lwt.t
end

module Make (E: IrminBase.S) (C: S) = struct

  open Lwt

  let debug fmt = IrminLog.debug "IO-FD" fmt

  include E

  type channel = C.t

  let read_channel chan =
    debug "read_channel %s" (C.name chan);
    lwt len = C.read_length chan in
    debug "read_ba len:%d ..." len;
    lwt ba = C.read_ba chan len in
    debug "... read_ba [OK]";
    let buf = IrminBuffer.of_ba ba in
    IrminBuffer.dump buf;
    return (E.get buf)

  let write_channel fd t =
    let len = E.sizeof t in
    debug "write_channel %s len:%d %s" (C.name fd) len (E.pretty t);
    let buf = IrminBuffer.create len in
    E.set buf t;
    lwt () = C.write_length fd len in
    debug "write_ba len:%d ..." len;
    lwt () = C.write_ba fd (IrminBuffer.to_ba buf) in
    debug "... write_ba [OK]";
    IrminBuffer.dump buf;
    return ()

end

module Lwt_unix = struct

  open Lwt

  type t = {
    fd  : Lwt_unix.file_descr;
    name: string;
  }

  let debug fmt = IrminLog.debug "IO-LWT" fmt

  let name t = t.name

  let channel t = t.fd

  let close t = Lwt_unix.close t.fd

  let read_string t len =
    debug "read_string %s len:%d" t.name len;
    let str = String.create len in
    let rec rread fd str ofs len =
      lwt n = Lwt_unix.read fd str ofs len in
      debug " ... read_string n:%d" n;
      if n = 0 then raise_lwt End_of_file
      else if n < len then rread fd str (ofs + n) (len - n)
      else return () in
    lwt () = rread t.fd str 0 len in
    return str

  let write_string t str =
    debug "write_string %s %S" t.name str;
    let rec rwrite fd str ofs len =
      lwt n = Lwt_unix.write fd str ofs len in
      if n = 0 then raise_lwt End_of_file
      else if n < len then rwrite fd str (ofs + n) (len - n)
      else return () in
    rwrite t.fd str 0 (String.length str)

  let read_ba t len =
    debug "read_ba %s len:%d" t.name len;
    let buf = IrminBuffer.create_ba len in
    let rec rread fd buf ofs len =
      debug "rread ofs=%d len=%d" ofs len;
      lwt n = Lwt_bytes.read fd buf ofs len in
      if n = 0 then raise_lwt End_of_file
      else if n < len then rread fd buf (ofs + n) (len - n)
      else return () in
    lwt () = rread t.fd buf 0 len in
    IrminBuffer.dump_ba buf;
    return buf

  let write_ba t ba =
    debug "write_ba %s" t.name;
    IrminBuffer.dump_ba ba;
    let rec rwrite fd buf ofs len =
      debug " ... write_buf %d" len;
      lwt n = Lwt_bytes.write fd buf ofs len in
      if n = 0 then raise_lwt End_of_file
      else if n < len then rwrite fd buf (ofs + n) (len - n)
      else return () in
    rwrite t.fd ba 0 (Bigarray.Array1.dim ba)

  let read_length t =
    debug "read_length %S" t.name;
    lwt str = read_string t 4 in
    let len = EndianString.BigEndian.get_int32 str 0 in
    debug " ... read_length: %ld" len;
    return (Int32.to_int len)

  let write_length t len =
    debug "write_length %s %dl" t.name len;
    let str = String.create 4 in
    EndianString.BigEndian.set_int32 str 0 (Int32.of_int len);
    lwt () = write_string t str in
    debug " ... write_length";
    return ()

  let write_unit t =
    write_string t "U"

  let read_unit t =
    lwt str = read_string t 1 in
    assert (str = "U");
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
    lwt () = Lwt_unix.connect fd (Lwt_unix.ADDR_UNIX file) in
    return (create fd file)

end
