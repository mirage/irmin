(*
 * Copyright (c) 2013-2020 Thomas Gazagnaire <thomas@gazagnaire.org>
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

let src = Logs.Src.create "irmin.layers.io" ~doc:"IO for irmin-layers"

module Log = (val Logs.src_log src : Logs.LOG)

module type S = sig
  type t

  val v : string -> t Lwt.t

  include S.CLOSEABLE with type _ t := t

  val read_flip : t -> bool Lwt.t
  val write_flip : bool -> t -> unit Lwt.t
end

module IO = struct
  type t = { file : string; fd : Lwt_unix.file_descr }

  let lseek ~offset t =
    Lwt_unix.lseek t.fd offset Lwt_unix.SEEK_SET >>= fun off ->
    if off <> offset then Lwt.fail_with "invalid lseek" else Lwt.return_unit

  let write ~offset t buf =
    lseek ~offset t >>= fun () ->
    let len = Bytes.length buf in
    Lwt_unix.write t.fd buf 0 len >>= fun n ->
    if n <> len then Lwt.fail_with "invalid write" else Lwt.return_unit

  let read ~offset t buf =
    lseek ~offset t >>= fun () ->
    let len = Bytes.length buf in
    Lwt_unix.read t.fd buf 0 len >>= fun n ->
    if n <> len then Lwt.fail_with "invalid read" else Lwt.return_unit

  let close t = Lwt_unix.close t.fd

  let read_flip t =
    let buf = Bytes.create 1 in
    read ~offset:0 t buf >>= fun () ->
    let ch = Bytes.get buf 0 in
    match int_of_char ch with
    | 0 -> Lwt.return_false
    | 1 -> Lwt.return_true
    | d -> Lwt.fail_with ("corrupted flip file " ^ string_of_int d)

  let write_flip flip t =
    let buf = Bytes.make 1 (char_of_int (if flip then 1 else 0)) in
    write ~offset:0 t buf

  let v file =
    match Sys.file_exists file with
    | false ->
        Lwt_unix.openfile file Lwt_unix.[ O_CREAT; O_RDWR; O_CLOEXEC ] 0o644
        >>= fun fd ->
        let t = { file; fd } in
        write_flip true t >|= fun () -> t
    | true ->
        Lwt_unix.openfile file Lwt_unix.[ O_EXCL; O_RDWR; O_CLOEXEC ] 0o644
        >|= fun fd -> { file; fd }
end

module Lock = struct
  type t = { file : string; fd : Lwt_unix.file_descr }

  let v file =
    let pid = string_of_int (Unix.getpid ()) in
    Lwt_unix.openfile file Unix.[ O_CREAT; O_WRONLY; O_TRUNC ] 0o644
    >>= fun fd ->
    Lwt_unix.write_string fd pid 0 (String.length pid) >>= fun n ->
    if n <> String.length pid then Lwt.fail_with "invalid write for lock file"
    else Lwt.return { file; fd }

  let test = Sys.file_exists
  let unlink = Lwt_unix.unlink
  let close { fd; file } = Lwt_unix.close fd >>= fun () -> unlink file
end
