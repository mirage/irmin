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

  (* NOTE following copied from Index_unix.Raw *)

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
  type create_error = [ `Io_misc of misc_error ]

  type open_error =
    [ `Io_misc of misc_error | `No_such_file_or_directory | `Is_a_directory ]

  type read_error =
    [ `Io_misc of misc_error | `Read_out_of_bounds | `Read_on_closed ]

  type write_error =
    [ `Io_misc of misc_error | `Ro_not_allowed | `Write_on_closed ]

  type close_error = [ `Io_misc of misc_error | `Double_close ]

  type mkdir_error =
    [ `Io_misc of misc_error | `File_exists | `No_such_file_or_directory ]

  type move_file_error = [ `Io_misc of misc_error ]

  (* the interface indicates that we need to catch double close errors; so type t needs
     some field to record whether the instance has been closed; we also need a field for
     "readonly" (to avoid querying the fd for this info); also for path (because of "path"
     function below) *)
  type t = {
    fd : Unix.file_descr;
    mutable closed : bool;
    readonly : bool;
    path : string;
  }

  exception Read_error of read_error
  exception Write_error of write_error

  (* default permissions which are required by some functions *)

  let default_create_perm = 0o660
  let default_open_perm = 0o660
  let default_mkdir_perm = 0o770 (* or 0o777? *)

  (* FIXME meaning of overwrite? we assume it means "overwrite: if the file exists,
     truncate it and use the truncated file instead"; otherwise, if overwrite=false, and
     we try to create a file that already exists, we fail with EEXIST *)
  let create ~path ~overwrite : (t, [> create_error ]) result =
    try
      (* NOTE there is a race condition between checking for file existence, and acting on
         that information; we assume we are not racing with others on this path *)
      let exists = Sys.file_exists path in
      match exists with
      | false ->
          (* just create a file where there wasn't one before *)
          let fd =
            Unix.(
              openfile path
                [ O_CREAT; O_RDWR; O_EXCL; O_CLOEXEC ]
                default_create_perm)
          in
          Ok { fd; closed = false; readonly = false; path }
      | true -> (
          (* file already exists; should we overwrite? *)
          match overwrite with
          | true ->
              (* open the file, with O_TRUNC flag; assumes we have the right permissions
                 etc *)
              let fd =
                Unix.(
                  openfile path
                    [ O_RDWR; O_CLOEXEC; O_TRUNC ]
                    default_create_perm)
              in
              Ok { fd; closed = false; readonly = false; path }
          | false ->
              (* the file already exists, and we can't overwrite it *)
              [%log.warn
                "%s: attempt to create a path %s that already exists \
                 (overwrite is false)"
                __FILE__ path];
              raise
                Unix.(
                  Unix_error
                    ( EEXIST,
                      (* second component is function name *)
                      Printf.sprintf "%s: create" __FILE__,
                      (* third is parameters *)
                      Printf.sprintf "path=%s; overwrite=%b" path overwrite )))
    with Unix.Unix_error (e, s1, s2) -> Error (`Io_misc (e, s1, s2))

  let open_ ~path ~readonly : (t, [> open_error ]) result =
    try
      let ok = Sys.file_exists path in
      assert ok;
      let mode = Unix.(if readonly then O_RDONLY else O_RDWR) in
      let fd = Unix.(openfile path [ mode; O_CLOEXEC ] default_open_perm) in
      Ok { fd; closed = false; readonly; path }
    with Unix.Unix_error (e, s1, s2) -> Error (`Io_misc (e, s1, s2))

  let close t : (unit, [> close_error ]) result =
    try
      match t.closed with
      | true -> Error `Double_close
      | false ->
          t.closed <- true;
          Unix.close t.fd;
          Ok ()
    with Unix.Unix_error (e, s1, s2) -> Error (`Io_misc (e, s1, s2))

  let write_string t ~off s : (unit, [> write_error ]) result =
    try
      match (t.closed, t.readonly) with
      | true, _ -> Error `Write_on_closed
      | _, true -> Error `Ro_not_allowed
      | _ ->
          (* really_write and following do not mutate the given buffer, so
             Bytes.unsafe_of_string is actually safe *)
          let buf = Bytes.unsafe_of_string s in
          let () = Util.really_write t.fd off buf 0 (Bytes.length buf) in
          Ok ()
    with Unix.Unix_error (e, s1, s2) -> Error (`Io_misc (e, s1, s2))

  (* NOTE copied from write_string above; ensure the code for both functions remains in
     sync *)
  let write_exn t ~off s : unit =
    try
      match (t.closed, t.readonly) with
      | true, _ -> raise (Write_error `Write_on_closed)
      | _, true -> raise (Write_error `Ro_not_allowed)
      | _ ->
          (* really_write and following do not mutate the given buffer, so
             Bytes.unsafe_of_string is actually safe *)
          let buf = Bytes.unsafe_of_string s in
          let () = Util.really_write t.fd off buf 0 (Bytes.length buf) in
          ()
    with Unix.Unix_error (e, s1, s2) ->
      raise (Write_error (`Io_misc (e, s1, s2)))

  let fsync t : (unit, [> write_error ]) result =
    try
      match (t.closed, t.readonly) with
      | true, _ -> Error `Write_on_closed
      | _, true -> Error `Ro_not_allowed
      | _ ->
          Unix.fsync t.fd;
          Ok ()
    with Unix.Unix_error (e, s1, s2) -> Error (`Io_misc (e, s1, s2))

  let read_to_string t ~off ~len =
    try
      match t.closed with
      | true -> Error `Read_on_closed
      | false -> (
          let buf = Bytes.create len in
          let nread = Util.really_read t.fd off len buf in
          match nread = len with
          | false ->
              (* didn't manage to read the desired amount; in this case the interface seems to
                 require we return `Read_out_of_bounds FIXME check this, because it is unusual
                 - the normal API allows return of a short string *)
              Error `Read_out_of_bounds
          | true ->
              (* we just created the buf; after we return it as a string, no-one has a handle
                 on it, so it cannot be mutated; so Bytes.unsafe_to_string is obviously safe
                 here *)
              Ok (Bytes.unsafe_to_string buf))
    with Unix.Unix_error (e, s1, s2) -> Error (`Io_misc (e, s1, s2))

  (* NOTE copied from read_to_string above; ensure the code for both functions remains in
     sync *)
  let read_exn t ~off ~len buf : unit =
    assert (len <= Bytes.length buf);
    try
      match t.closed with
      | true -> raise (Read_error `Read_on_closed)
      | false -> (
          let nread = Util.really_read t.fd off len buf in
          match nread = len with
          | false ->
              (* didn't manage to read the desired amount; in this case the interface seems to
                 require we return `Read_out_of_bounds FIXME check this, because it is unusual
                 - the normal API allows return of a short string *)
              raise (Read_error `Read_out_of_bounds)
          | true -> ())
    with Unix.Unix_error (e, s1, s2) ->
      raise (Read_error (`Io_misc (e, s1, s2)))

  let read_size t : (int63, [> read_error ]) result =
    try
      match t.closed with
      | true -> Error `Read_on_closed
      | false -> Ok Unix.LargeFile.((fstat t.fd).st_size |> Int63.of_int64)
    with Unix.Unix_error (e, s1, s2) ->
      raise (Read_error (`Io_misc (e, s1, s2)))

  let readonly t = t.readonly
  let path t = t.path

  let classify_path p =
    Unix.(
      try
        (* the interface assumes there are only two types of file FIXME check that
           assert(false) is the desired behaviour here; maybe add `Other as a file
           kind? *)
        match (stat p).st_kind with
        | S_REG -> `File
        | S_DIR -> `Directory
        | _ -> assert false
      with _ -> `No_such_file_or_directory)

  let page_size = 4096

  let move_file ~src ~dst : (unit, [> move_file_error ]) result =
    try
      Sys.rename src dst;
      Ok ()
    with Unix.Unix_error (e, s1, s2) -> Error (`Io_misc (e, s1, s2))

  let mkdir path : (unit, [> mkdir_error ]) result =
    try
      let exists = Sys.file_exists path in
      match exists with
      | true -> Error `File_exists
      | false -> (
          (* the interface has a `No_such_file_or_directory error, which presumably is for
             when the directory component of the path doesn't exist *)
          match Sys.file_exists (Filename.dirname path) with
          | false -> Error `No_such_file_or_directory
          | true ->
              (* parent exists, entry doesn't; can just create *)
              Unix.mkdir path default_mkdir_perm;
              Ok ())
    with Unix.Unix_error (e, s1, s2) -> Error (`Io_misc (e, s1, s2))
end
