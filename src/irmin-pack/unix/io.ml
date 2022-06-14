(*
 * Copyright (c) 2022-2022 Tarides <contact@tarides.com>
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

(* File utils, taken from index.unix package *)
module Util = struct
  module Syscalls = Index_unix.Syscalls

  let really_write fd fd_offset buffer buffer_offset length =
    let rec aux fd_offset buffer_offset length =
      let w = Syscalls.pwrite ~fd ~fd_offset ~buffer ~buffer_offset ~length in
      if w = 0 || w = length then ()
      else
        (aux [@tailcall])
          (fd_offset ++ Int63.of_int w)
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
          (fd_offset ++ Int63.of_int r)
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

  type create_error = [ `Io_misc of misc_error | `File_exists ]

  type open_error =
    [ `Io_misc of misc_error | `No_such_file_or_directory | `Not_a_file ]

  type read_error =
    [ `Io_misc of misc_error
    | `Read_out_of_bounds
    | `Read_on_closed
    | `Invalid_argument ]

  type write_error =
    [ `Io_misc of misc_error | `Ro_not_allowed | `Write_on_closed ]

  type close_error = [ `Io_misc of misc_error | `Double_close ]
  type move_file_error = [ `Io_misc of misc_error ]

  type mkdir_error =
    [ `Io_misc of misc_error
    | `File_exists
    | `No_such_file_or_directory
    | `Invalid_parent_directory ]

  let raise_misc_error (x, y, z) = raise (Unix.Unix_error (x, y, z))

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

  let create ~path ~overwrite : (t, [> create_error ]) result =
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
          | false -> Error `File_exists)
    with Unix.Unix_error (e, s1, s2) -> Error (`Io_misc (e, s1, s2))

  let open_ ~path ~readonly : (t, [> open_error ]) result =
    match classify_path path with
    | `Directory | `Other -> Error `Not_a_file
    | `No_such_file_or_directory -> Error `No_such_file_or_directory
    | `File -> (
        let mode = Unix.(if readonly then O_RDONLY else O_RDWR) in
        try
          let fd = Unix.(openfile path [ mode; O_CLOEXEC ] default_open_perm) in
          Ok { fd; closed = false; readonly; path }
        with Unix.Unix_error (e, s1, s2) -> Error (`Io_misc (e, s1, s2)))

  let close t : (unit, [> close_error ]) result =
    match t.closed with
    | true -> Error `Double_close
    | false -> (
        t.closed <- true;
        try
          Unix.close t.fd;
          Ok ()
        with Unix.Unix_error (e, s1, s2) -> Error (`Io_misc (e, s1, s2)))

  let write_exn t ~off s : unit =
    match (t.closed, t.readonly) with
    | true, _ -> raise (Errors_base.Io_error `Write_on_closed)
    | _, true -> raise Errors_base.RO_not_allowed
    | _ ->
        (* really_write and following do not mutate the given buffer, so
           Bytes.unsafe_of_string is actually safe *)
        let buf = Bytes.unsafe_of_string s in
        let len = Bytes.length buf in
        let () = Util.really_write t.fd off buf 0 len in
        Index.Stats.add_write len;
        ()

  let write_string t ~off s : (unit, [> write_error ]) result =
    try Ok (write_exn t ~off s) with
    | Errors_base.Io_error (`Write_on_closed as e) -> Error e
    | Errors_base.RO_not_allowed -> Error `Ro_not_allowed
    | Unix.Unix_error (e, s1, s2) -> Error (`Io_misc (e, s1, s2))

  let fsync t : (unit, [> write_error ]) result =
    match (t.closed, t.readonly) with
    | true, _ -> Error `Write_on_closed
    | _, true -> Error `Ro_not_allowed
    | _ -> (
        try
          Unix.fsync t.fd;
          Ok ()
        with Unix.Unix_error (e, s1, s2) -> Error (`Io_misc (e, s1, s2)))

  let read_exn t ~off ~len buf : unit =
    if len > Bytes.length buf then
      raise (Errors_base.Io_error `Invalid_argument);
    match t.closed with
    | true -> raise (Errors_base.Io_error `Read_on_closed)
    | false ->
        let nread = Util.really_read t.fd off len buf in
        Index.Stats.add_read nread;
        if nread <> len then
          (* didn't manage to read the desired amount; in this case the interface seems to
             require we return `Read_out_of_bounds FIXME check this, because it is unusual
             - the normal API allows return of a short string *)
          raise (Errors_base.Io_error `Read_out_of_bounds)

  let read_to_string t ~off ~len =
    let buf = Bytes.create len in
    try
      read_exn t ~off ~len buf;
      Ok (Bytes.unsafe_to_string buf)
    with
    | Errors_base.Io_error
        ((`Invalid_argument | `Read_on_closed | `Read_out_of_bounds) as e) ->
        Error e
    | Unix.Unix_error (e, s1, s2) -> Error (`Io_misc (e, s1, s2))

  let read_size t : (int63, [> read_error ]) result =
    match t.closed with
    | true -> Error `Read_on_closed
    | false -> (
        try Ok Unix.LargeFile.((fstat t.fd).st_size |> Int63.of_int64)
        with Unix.Unix_error (e, s1, s2) -> Error (`Io_misc (e, s1, s2)))

  let readonly t = t.readonly
  let path t = t.path
  let page_size = 4096

  let move_file ~src ~dst : (unit, [> move_file_error ]) result =
    try
      Sys.rename src dst;
      Ok ()
    with Unix.Unix_error (e, s1, s2) -> Error (`Io_misc (e, s1, s2))

  let mkdir path : (unit, [> mkdir_error ]) result =
    match (classify_path (Filename.dirname path), classify_path path) with
    | `Directory, `No_such_file_or_directory -> (
        try
          Unix.mkdir path default_mkdir_perm;
          Ok ()
        with Unix.Unix_error (e, s1, s2) -> Error (`Io_misc (e, s1, s2)))
    | `Directory, (`File | `Directory | `Other) -> Error `File_exists
    | `No_such_file_or_directory, `No_such_file_or_directory ->
        Error `No_such_file_or_directory
    | _ -> Error `Invalid_parent_directory
end
