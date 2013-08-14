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
open IrminTypes

module Channel: CHANNEL with type t = Lwt_unix.file_descr = struct

  (* From https://github.com/djs55/ocaml-vnc/blob/master/lib/rfb_lwt.ml *)
  type t = Lwt_unix.file_descr

  let close = Lwt_unix.close

  let read_string fd n =
    let buf = String.make n '\000' in
    let rec rread fd buf ofs len =
      lwt n = Lwt_unix.read fd buf ofs len in
      if n = 0 then raise End_of_file;
      if n < len then rread fd buf (ofs + n) (len - n) else return () in
    lwt () = rread fd buf 0 n in
    return buf

  let write_string fd buf =
    let rec rwrite fd buf ofs len =
      lwt n = Lwt_unix.write fd buf ofs len in
      if n = 0 then raise End_of_file;
      if n < len then rwrite fd buf (ofs + n) (len - n) else return () in
    lwt () = rwrite fd buf 0 (String.length buf) in
    return ()

  let read fd n =
    lwt result = read_string fd n in
    return (Cstruct.of_string result)

  let write fd buf =
    write_string fd (Cstruct.to_string buf)

end

module SHA1 = IrminKey.SHA1(Channel)

include IrminProtocol.Make(Channel)(SHA1)

module Key_store = IrminMemory.Key_store(Key)

module Value_store = IrminMemory.Value_store(Key)(Value)

module Tag_store = IrminMemory.Tag_store(Tag)(Key)

module MemoryServer = Server(Key_store)(Tag_store)
