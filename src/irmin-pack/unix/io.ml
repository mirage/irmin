(*
 * Copyright (c) 2022-2023 Tarides <contact@tarides.com>
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

open! Import
open Io_intf
module Syscalls = Index_unix.Syscalls

(* File utils, taken from index.unix package.

   These functions need to read from a loop because the underlying
   implementation will not read/write more than a constant called
   [UNIX_BUFFER_SIZE]. *)
module Util = struct
  let really_write fd fd_offset buffer buffer_offset length =
    let rec aux fd_offset buffer_offset length =
      let w = Syscalls.pwrite ~fd ~fd_offset ~buffer ~buffer_offset ~length in
      if w = 0 || w = length then ()
      else
        (aux [@tailcall])
          Int63.Syntax.(fd_offset + Int63.of_int w)
          (buffer_offset + w) (length - w)
    in
    aux fd_offset buffer_offset length

  let really_read fd fd_offset length buffer =
    let rec aux fd_offset buffer_offset length =
      let r = Syscalls.pread ~fd ~fd_offset ~buffer ~buffer_offset ~length in
      if r = 0 then buffer_offset (* end of file *)
      else if r = length then buffer_offset + r
      else
        (aux [@tailcall])
          Int63.Syntax.(fd_offset + Int63.of_int r)
          (buffer_offset + r) (length - r)
    in
    aux fd_offset 0 length
end

module type S = S

module Unix = struct
  type misc_error = Unix.error * string * string

  let unix_error_t =
    Irmin.Type.(map string (fun _str -> assert false) Unix.error_message)

  let misc_error_t = Irmin.Type.(triple unix_error_t string string)

  type create_error = [ `Io_misc of misc_error | `File_exists of string ]

  type open_error =
    [ `Io_misc of misc_error
    | `No_such_file_or_directory of string
    | `Not_a_file ]

  type read_error =
    [ `Io_misc of misc_error
    | `Read_out_of_bounds
    | `Closed
    | `Invalid_argument ]

  type write_error = [ `Io_misc of misc_error | `Ro_not_allowed | `Closed ]
  type close_error = [ `Io_misc of misc_error | `Double_close ]

  type mkdir_error =
    [ `Io_misc of misc_error
    | `File_exists of string
    | `No_such_file_or_directory of string
    | `Invalid_parent_directory ]

  let raise_misc_error (x, y, z) = raise (Unix.Unix_error (x, y, z))

  let catch_misc_error f =
    try Ok (f ())
    with Unix.Unix_error (e, s1, s2) -> Error (`Io_misc (e, s1, s2))

  type t = {
    fd : Unix.file_descr;
    mutable closed : bool;
    readonly : bool;
    path : string;
  }

  let classify_path p =
    Unix.(
      try
        match (stat p).st_kind with
        | S_REG -> `File
        | S_DIR -> `Directory
        | _ -> `Other
      with _ -> `No_such_file_or_directory)

  let default_create_perm = 0o644
  let default_open_perm = 0o644
  let default_mkdir_perm = 0o755

  let create ~path ~overwrite =
    try
      match Sys.file_exists path with
      | false ->
          let fd =
            Unix.(
              openfile path
                [ O_CREAT; O_RDWR; O_EXCL; O_CLOEXEC ]
                default_create_perm)
          in
          Ok { fd; closed = false; readonly = false; path }
      | true -> (
          match overwrite with
          | true ->
              (* The file exists, truncate it and use it. An exception will be
                 triggered if we don't have the permissions *)
              let fd =
                Unix.(
                  openfile path
                    [ O_RDWR; O_CLOEXEC; O_TRUNC ]
                    default_create_perm)
              in
              Ok { fd; closed = false; readonly = false; path }
          | false -> Error (`File_exists path))
    with
    | Unix.Unix_error (e, s1, s2) -> Error (`Io_misc (e, s1, s2))
    | Sys_error _ -> assert false

  let open_ ~path ~readonly =
    match classify_path path with
    | `Directory | `Other -> Error `Not_a_file
    | `No_such_file_or_directory -> Error (`No_such_file_or_directory path)
    | `File -> (
        let mode = Unix.(if readonly then O_RDONLY else O_RDWR) in
        try
          let fd = Unix.(openfile path [ mode; O_CLOEXEC ] default_open_perm) in
          Ok { fd; closed = false; readonly; path }
        with Unix.Unix_error (e, s1, s2) -> Error (`Io_misc (e, s1, s2)))

  let close t =
    match t.closed with
    | true -> Error `Double_close
    | false -> (
        t.closed <- true;
        (* mark [t] as closed, even if [Unix.close] fails, since it is recommended
           to not retry after an error. see: https://man7.org/linux/man-pages/man2/close.2.html *)
        try
          Unix.close t.fd;
          Ok ()
        with Unix.Unix_error (e, s1, s2) -> Error (`Io_misc (e, s1, s2)))

  let write_exn t ~off ~len s =
    if String.length s < len then raise (Errors.Pack_error `Invalid_argument);
    match (t.closed, t.readonly) with
    | true, _ -> raise Errors.Closed
    | _, true -> raise Errors.RO_not_allowed
    | _ ->
        (* Bytes.unsafe_of_string usage: s has shared ownership; we assume that
           Util.really_write does not mutate buf (i.e., only needs shared ownership). This
           usage is safe. *)
        let buf = Bytes.unsafe_of_string s in
        let () = Util.really_write t.fd off buf 0 len in
        Index.Stats.add_write len;
        ()

  let write_string t ~off s =
    let len = String.length s in
    try Ok (write_exn t ~off ~len s) with
    | Errors.Closed -> Error `Closed
    | Errors.RO_not_allowed -> Error `Ro_not_allowed
    | Unix.Unix_error (e, s1, s2) -> Error (`Io_misc (e, s1, s2))

  let fsync t =
    match (t.closed, t.readonly) with
    | true, _ -> Error `Closed
    | _, true -> Error `Ro_not_allowed
    | _ -> (
        try
          Unix.fsync t.fd;
          Ok ()
        with Unix.Unix_error (e, s1, s2) -> Error (`Io_misc (e, s1, s2)))

  let read_exn t ~off ~len buf =
    if len > Bytes.length buf then raise (Errors.Pack_error `Invalid_argument);
    match t.closed with
    | true -> raise Errors.Closed
    | false ->
        let nread = Util.really_read t.fd off len buf in
        Index.Stats.add_read nread;
        if nread <> len then
          (* didn't manage to read the desired amount; in this case the interface seems to
             require we return `Read_out_of_bounds FIXME check this, because it is unusual
             - the normal API allows return of a short string *)
          raise (Errors.Pack_error `Read_out_of_bounds)

  let read_to_string t ~off ~len =
    let buf = Bytes.create len in
    try
      read_exn t ~off ~len buf;
      (* Bytes.unsafe_to_string usage: buf is local to this function, so uniquely
         owned. We assume read_exn returns unique ownership of buf to this function. Then
         at the call to Bytes.unsafe_to_string we give up unique ownership of buf for
         ownership of the string. This is safe. *)
      Ok (Bytes.unsafe_to_string buf)
    with
    | Errors.Pack_error ((`Invalid_argument | `Read_out_of_bounds) as e) ->
        Error e
    | Errors.Closed -> Error `Closed
    | Unix.Unix_error (e, s1, s2) -> Error (`Io_misc (e, s1, s2))

  let page_size = 4096

  let read_all_to_string t =
    let open Result_syntax in
    let* () = if t.closed then Error `Closed else Ok () in
    let buf = Buffer.create 0 in
    let len = page_size in
    let bytes = Bytes.create len in
    let rec aux ~off =
      let nread =
        Syscalls.pread ~fd:t.fd ~fd_offset:off ~buffer:bytes ~buffer_offset:0
          ~length:len
      in
      if nread > 0 then (
        Index.Stats.add_read nread;
        Buffer.add_subbytes buf bytes 0 nread;
        if nread = len then aux ~off:Int63.(add off (of_int nread)))
    in
    try
      aux ~off:Int63.zero;
      Ok (Buffer.contents buf)
    with Unix.Unix_error (e, s1, s2) -> Error (`Io_misc (e, s1, s2))

  let read_size t =
    match t.closed with
    | true -> Error `Closed
    | false -> (
        try Ok Unix.LargeFile.((fstat t.fd).st_size |> Int63.of_int64)
        with Unix.Unix_error (e, s1, s2) -> Error (`Io_misc (e, s1, s2)))

  let size_of_path s =
    let open Result_syntax in
    let* io = open_ ~path:s ~readonly:true in
    let res =
      match read_size io with
      | Error `Closed -> assert false
      | Error (`Io_misc _) as x -> x
      | Ok _ as x -> x
    in
    match close io with
    | Error `Double_close -> assert false
    | Error (`Io_misc _) as x -> x
    | Ok () -> res

  let readonly t = t.readonly
  let path t = t.path

  let move_file ~src ~dst =
    try
      Sys.rename src dst;
      Ok ()
    with Sys_error msg -> Error (`Sys_error msg)

  let copy_file ~src ~dst =
    let cmd = Filename.quote_command "cp" [ "-p"; src; dst ] in
    match Sys.command cmd with
    | 0 -> Ok ()
    | n -> Error (`Sys_error (Int.to_string n))

  let mkdir path =
    match (classify_path (Filename.dirname path), classify_path path) with
    | `Directory, `No_such_file_or_directory -> (
        try
          Unix.mkdir path default_mkdir_perm;
          Ok ()
        with Unix.Unix_error (e, s1, s2) -> Error (`Io_misc (e, s1, s2)))
    | `Directory, (`File | `Directory | `Other) -> Error (`File_exists path)
    | `No_such_file_or_directory, `No_such_file_or_directory ->
        Error (`No_such_file_or_directory path)
    | _ -> Error `Invalid_parent_directory

  let unlink path =
    try
      Sys.remove path;
      Ok ()
    with Sys_error msg -> Error (`Sys_error msg)

  let unlink_dont_wait ~on_exn path =
    Lwt.dont_wait (fun () -> Lwt_unix.unlink path) on_exn
end
