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

open IrminTypes

type t =
  [ `File of string
  | `Local of string
  | `Remote of (string * int) ]

let fd = function
  | `File f  -> Lwt_unix.(openfile f [O_RDWR; O_NONBLOCK] 0x644)
  | `Local f ->
    let fd = Lwt_unix.(socket PF_UNIX SOCK_STREAM 0) in
    Lwt_unix.(bind fd (ADDR_UNIX f));
    Lwt.return fd
  | `Remote (host,port) ->
    let fd = Lwt_unix.(socket PF_UNIX SOCK_STREAM 0) in
    lwt host = Lwt_unix.gethostbyname host in
    let addr = host.Lwt_unix.h_addr_list.(0) in
    Lwt_unix.(bind fd (ADDR_INET (addr,port)));
    Lwt.return fd

module File = struct

  let init f =
    if Sys.file_exists f then (
      Printf.printf "%s already exists" f;
      failwith "init"
    ) else
      lwt () = IrminLwt.Disk.init f in
      Lwt.return ()

end

let init = function
  | `File f   -> Lwt_unix.run (File.init f)
  | `Local _  -> failwith "TODO"
  | `Remote _ -> failwith "TODO"

let add _ =
  failwith "TODO"

let watch _ =
  failwith "TODO"

let take _ =
  failwith "TODO"

let peek _ =
  failwith "TODO"

let dump _ =
  failwith "TODO"

let pull _ =
  failwith "TODO"

let push _ =
  failwith "TODO"

let clone _ =
  failwith "TODO"
