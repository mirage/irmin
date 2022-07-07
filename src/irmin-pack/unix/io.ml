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
    [ `Io_misc of misc_error | `No_such_file_or_directory | `Not_a_file ]

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
    | `No_such_file_or_directory
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
    | `No_such_file_or_directory -> Error `No_such_file_or_directory
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
        (* really_write and following do not mutate the given buffer, so
           Bytes.unsafe_of_string is actually safe *)
        let buf = Bytes.unsafe_of_string s (* safe - see comment 1 line above *) in
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
      Ok (Bytes.unsafe_to_string buf) (* safe: buf local, not leaked, not modified after return *)
    with
    | Errors.Pack_error ((`Invalid_argument | `Read_out_of_bounds) as e) ->
        Error e
    | Errors.Closed -> Error `Closed
    | Unix.Unix_error (e, s1, s2) -> Error (`Io_misc (e, s1, s2))

  let read_size t =
    match t.closed with
    | true -> Error `Closed
    | false -> (
        try Ok Unix.LargeFile.((fstat t.fd).st_size |> Int63.of_int64)
        with Unix.Unix_error (e, s1, s2) -> Error (`Io_misc (e, s1, s2)))

  let readonly t = t.readonly
  let path t = t.path
  let page_size = 4096

  let move_file ~src ~dst =
    try
      Sys.rename src dst;
      Ok ()
    with Sys_error msg -> Error (`Sys_error msg)

  let mkdir path =
    match (classify_path (Filename.dirname path), classify_path path) with
    | `Directory, `No_such_file_or_directory -> (
        try
          Unix.mkdir path default_mkdir_perm;
          Ok ()
        with Unix.Unix_error (e, s1, s2) -> Error (`Io_misc (e, s1, s2)))
    | `Directory, (`File | `Directory | `Other) -> Error (`File_exists path)
    | `No_such_file_or_directory, `No_such_file_or_directory ->
        Error `No_such_file_or_directory
    | _ -> Error `Invalid_parent_directory

  let unlink path =
    try
      Sys.remove path;
      Ok ()
    with Sys_error msg -> Error (`Sys_error msg)

  (* Async using fork/waitpid*)

  module Exit = struct
    let proc_list = ref []
    let m = Mutex.create ()

    let add gc =
      Mutex.lock m;
      proc_list := gc :: !proc_list;
      Mutex.unlock m

    let remove gc =
      Mutex.lock m;
      proc_list := List.filter (fun gc' -> gc <> gc') !proc_list;
      Mutex.unlock m

    let clean_up () =
      List.iter
        (fun gc ->
          try Unix.kill gc 9
          with Unix.Unix_error (e, s1, s2) ->
            [%log.warn
              "Killing gc process with pid %d failed with error (%s, %s, %s)" gc
                (Unix.error_message e) s1 s2])
        !proc_list
  end

  (* Register function to be called when process terminates. If there is a gc
     process running, make sure to terminate it. *)
  let () = at_exit Exit.clean_up

  type status = [ `Running | `Success | `Cancelled | `Failure of string ]
  [@@deriving irmin]

  type task = { pid : int; mutable status : status }

  let async f =
    Stdlib.flush_all ();
    match Lwt_unix.fork () with
    | 0 ->
        Lwt_main.Exit_hooks.remove_all ();
        Lwt_main.abandon_yielded_and_paused ();
        f ();
        (* Once the gc is finished, the child process kills itself to
           avoid calling at_exit functions in upstream code. *)
        Unix.kill (Unix.getpid ()) 9;
        assert false (* unreachable *)
    | pid ->
        Exit.add pid;
        { pid; status = `Running }

  let status_of_process_status = function
    | Lwt_unix.WSIGNALED -7 ->
        `Success (* the child is killing itself when it's done *)
    | Lwt_unix.WSIGNALED n -> `Failure (Fmt.str "Signaled %d" n)
    | Lwt_unix.WEXITED n -> `Failure (Fmt.str "Exited %d" n)
    | Lwt_unix.WSTOPPED n -> `Failure (Fmt.str "Stopped %d" n)

  let cancel t =
    let () =
      match t.status with
      | `Running ->
          let pid, _ = Unix.waitpid [ Unix.WNOHANG ] t.pid in
          (* Do not block if no child has died yet. In this case the waitpid
             returns immediately with a pid equal to 0. *)
          if pid = 0 then (
            Unix.kill t.pid 9;
            Exit.remove t.pid)
      | _ -> ()
    in
    t.status <- `Cancelled

  let status t : status =
    match t.status with
    | `Running ->
        let pid, status = Unix.waitpid [ Unix.WNOHANG ] t.pid in
        (* Do not block if no child has died yet. In this case the waitpid
           returns immediately with a pid equal to 0. *)
        if pid = 0 then `Running
        else
          let s = status_of_process_status status in
          Exit.remove pid;
          t.status <- s;
          s
    | s -> s

  let await t =
    match t.status with
    | `Running ->
        let+ pid, status = Lwt_unix.waitpid [] t.pid in
        let s = status_of_process_status status in
        Exit.remove pid;
        t.status <- s;
        s
    | s -> Lwt.return s
end
