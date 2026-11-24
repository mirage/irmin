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

module Eio_temp = Eio
open! Irmin_pack_io.Import
module Eio = Eio_temp
module Errors = Irmin_pack_io.Errors

(* File utils, taken from index.unix package.

   These functions need to read from a loop because the underlying
   implementation will not read/write more than a constant called
   [UNIX_BUFFER_SIZE]. *)

module Util = struct
  let really_write fd file_offset buffer buffer_offset length =
    let cs = Cstruct.of_bytes ~off:buffer_offset ~len:length buffer in
    Eio.File.pwrite_all fd ~file_offset [ cs ]

  let really_read fd file_offset length buffer =
    let cs = Cstruct.create length in
    Eio.File.pread_exact fd ~file_offset [ cs ];
    Cstruct.blit_to_bytes cs 0 buffer 0 length
end

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

  (* [misc_error_of_eio exn err] is the Unix error of [err], raised by [exn].
     The Unix backends of Eio wrap most of their errors in
     [Eio_unix.Unix_error]. The other errors get the closest Unix error and the
     message of [exn]: a path outside the sandbox of a directory, or, with
     recent versions of Eio, a loop of symbolic links ([Eio.Fs.Symlink], which
     becomes [EIO] as older versions do not have it). *)
  let misc_error_of_eio exn : Eio.Exn.err -> misc_error = function
    | Eio.Exn.X (Eio_unix.Unix_error (e, s1, s2))
    | Eio.Fs.E
        ( Already_exists (Eio_unix.Unix_error (e, s1, s2))
        | Not_found (Eio_unix.Unix_error (e, s1, s2))
        | Permission_denied (Eio_unix.Unix_error (e, s1, s2)) ) ->
        (e, s1, s2)
    | err ->
        let e =
          match err with
          | Eio.Fs.E (Already_exists _) -> Unix.EEXIST
          | Eio.Fs.E (Not_found _) -> Unix.ENOENT
          | Eio.Fs.E (Permission_denied _) -> Unix.EACCES
          | Eio.Fs.E File_too_large -> Unix.EFBIG
          | _ -> Unix.EIO
        in
        (e, "", Printexc.to_string exn)

  (* [catch_eio f] is [f ()], or [Error (`Io_misc _)] if it raises [Eio.Io]. *)
  let catch_eio f =
    try f ()
    with Eio.Io (err, _) as exn ->
      Error (`Io_misc (misc_error_of_eio exn err))

  type file =
    | RO of Eio.File.ro_ty Eio.Resource.t
    | RW of Eio.File.rw_ty Eio.Resource.t

  let get_file_as_ro = function
    | RO file -> file
    | RW file -> (file :> Eio.File.ro_ty Eio.Resource.t)

  type t = {
    file : file;
    mutable closed : bool;
    path : Eio.Fs.dir_ty Eio.Path.t;
  }

  let classify_path path =
    match
      match Eio.Path.kind ~follow:true path with
      | `Not_found ->
          (* A dangling symbolic link: nothing can be created at its path. *)
          Eio.Path.kind ~follow:false path
      | kind -> kind
    with
    | `Regular_file -> `File
    | `Directory -> `Directory
    | `Not_found -> `No_such_file_or_directory
    | _ -> `Other
    | exception (Eio.Io _ as exn) ->
        (* For instance a loop of symbolic links. *)
        [%log.warn "Cannot classify %a: %a" Eio.Path.pp path Fmt.exn exn];
        `Other

  let readdir p = Eio.Path.read_dir p
  let default_create_perm = 0o644
  let default_mkdir_perm = 0o755

  let create ~sw ~path ~overwrite =
    catch_eio @@ fun () ->
    let create_new () =
      Ok
        {
          file =
            RW
              (Eio.Path.open_out ~sw ~create:(`Exclusive default_create_perm)
                 path);
          closed = false;
          path;
        }
    in
    let file_exists () = Error (`File_exists (Eio.Path.native_exn path)) in
    match (Eio.Path.kind ~follow:false path, overwrite) with
    | `Not_found, _ -> create_new ()
    | `Regular_file, true ->
        (* The file exists, truncate it and use it. An exception will be
           triggered if we don't have the permissions *)
        let file =
          RW
            (Eio.Path.open_out ~sw ~create:(`Or_truncate default_create_perm)
               path)
        in
        Ok { file; closed = false; path }
    | `Symbolic_link, true -> (
        match Eio.Path.kind ~follow:true path with
        | `Regular_file | `Not_found ->
            (* Replace the link, even a dangling one, rather than truncate its
               target, which may belong to another store. *)
            Eio.Path.unlink path;
            create_new ()
        | _ -> file_exists ())
    (* A directory, or another kind of file, is not overwritten. *)
    | _ -> file_exists ()

  (* In read-write mode, a symbolic link is refused. Files replaced by a rename
     (control file, index merges) would replace the link, while the others
     would be written through it: the store would drift apart from the store
     the links point to, and corrupt it. *)
  let open_ ~sw ~path ~readonly =
    catch_eio @@ fun () ->
    match Eio.Path.kind ~follow:true path with
    | `Not_found ->
        Error (`No_such_file_or_directory (Eio.Path.native_exn path))
    | `Regular_file -> (
        match (readonly, Eio.Path.kind ~follow:false path) with
        | true, _ ->
            Ok { file = RO (Eio.Path.open_in ~sw path); closed = false; path }
        | false, `Symbolic_link ->
            [%log.err
              "Cannot open %a read-write: it is a symbolic link" Eio.Path.pp
                path];
            Error `Not_a_file
        | false, _ ->
            let file = RW (Eio.Path.open_out ~sw ~create:`Never path) in
            Ok { file; closed = false; path })
    | _ -> Error `Not_a_file

  let close t =
    match t.closed with
    | true -> Error `Double_close
    | false -> (
        t.closed <- true;
        (* mark [t] as closed, even if [Unix.close] fails, since it is recommended
           to not retry after an error. see: https://man7.org/linux/man-pages/man2/close.2.html *)
        try
          let file = get_file_as_ro t.file in
          Eio.Resource.close file;
          Ok ()
        with Unix.Unix_error (e, s1, s2) -> Error (`Io_misc (e, s1, s2)))

  let write_exn t ~off ~len s =
    if String.length s < len then raise (Errors.Pack_error `Invalid_argument);
    match (t.closed, t.file) with
    | true, _ -> raise Errors.Closed
    | _, RO _ -> raise Errors.RO_not_allowed
    | _, RW file ->
        (* Bytes.unsafe_of_string usage: s has shared ownership; we assume that
           Util.really_write does not mutate buf (i.e., only needs shared ownership). This
           usage is safe. *)
        let buf = Bytes.unsafe_of_string s in
        let () = Util.really_write file off buf 0 len in
        Index.Stats.add_write len;
        ()

  let write_string t ~off s =
    let len = String.length s in
    try Ok (write_exn t ~off ~len s) with
    | Errors.Closed -> Error `Closed
    | Errors.RO_not_allowed -> Error `Ro_not_allowed
    | Unix.Unix_error (e, s1, s2) -> Error (`Io_misc (e, s1, s2))

  let fsync t =
    match (t.closed, t.file) with
    | true, _ -> Error `Closed
    | _, RO _ -> Error `Ro_not_allowed
    | _, RW file -> (
        try
          Eio.File.sync file;
          Ok ()
        with Unix.Unix_error (e, s1, s2) -> Error (`Io_misc (e, s1, s2)))

  let read_exn t ~off ~len buf =
    if len > Bytes.length buf then raise (Errors.Pack_error `Invalid_argument);
    match t.closed with
    | true -> raise Errors.Closed
    | false -> (
        try
          let file = get_file_as_ro t.file in
          Util.really_read file off len buf;
          Index.Stats.add_read len
        with exn ->
          Printexc.print_backtrace stderr;
          raise exn)

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
    let cs = Cstruct.create len in
    let rec aux ~off =
      let file = get_file_as_ro t.file in
      let nread = Eio.File.pread file ~file_offset:off [ cs ] in
      if nread > 0 then (
        Index.Stats.add_read nread;
        Buffer.add_subbytes buf (Cstruct.to_bytes ~off:0 ~len:nread cs) 0 nread;
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
        try
          let file = get_file_as_ro t.file in
          Ok Eio.File.(stat file).size
        with Unix.Unix_error (e, s1, s2) -> Error (`Io_misc (e, s1, s2)))

  let size_of_path s =
    let open Result_syntax in
    Eio.Switch.run @@ fun sw ->
    let* io = open_ ~path:s ~readonly:true ~sw in
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

  let readonly t = match t.file with RO _ -> true | RW _ -> false
  let path t = t.path

  (* [catch_sys_error f] is [Ok (f ())], or [Error (`Sys_error _)] if it raises
     [Eio.Io]. *)
  let catch_sys_error f =
    try Ok (f ())
    with Eio.Io (err, _context) ->
      Error (`Sys_error (Fmt.str "%a" Eio.Exn.pp_err err))

  let move_file ~src ~dst = catch_sys_error @@ fun () -> Eio.Path.rename src dst

  let copy_file ~src ~dst =
    catch_sys_error @@ fun () ->
    let stats = Eio.Path.stat ~follow:true src in
    (match Eio.Path.kind ~follow:false dst with
    | `Symbolic_link ->
        (* Replace the link, rather than truncate its target, which may belong
           to another store. *)
        Eio.Path.unlink dst
    | _ -> ());
    Eio.Path.with_open_in src (fun in_flow ->
        Eio.Path.with_open_out ~create:(`Or_truncate stats.perm) dst
          (fun out_flow -> Eio.Flow.copy in_flow out_flow))

  let mkdir path =
    catch_eio @@ fun () ->
    let dirname, _ = Option.get @@ Eio.Path.split path in
    match
      (Eio.Path.kind ~follow:true dirname, Eio.Path.kind ~follow:false path)
    with
    | `Directory, `Not_found ->
        Eio.Path.mkdir ~perm:default_mkdir_perm path;
        Ok ()
    | `Directory, _ -> Error (`File_exists (Eio.Path.native_exn path))
    | `Not_found, `Not_found ->
        Error (`No_such_file_or_directory (Eio.Path.native_exn path))
    | _ -> Error `Invalid_parent_directory

  let rmdir path = Eio.Path.rmdir path
  let unlink path = catch_sys_error @@ fun () -> Eio.Path.unlink path

  let symlinks dir =
    catch_sys_error @@ fun () ->
    let is_symlink path = Eio.Path.kind ~follow:false path = `Symbolic_link in
    match Eio.Path.kind ~follow:false dir with
    | `Symbolic_link -> [ dir ]
    | `Directory ->
        Eio.Path.read_dir dir
        |> List.map (fun name -> Eio.Path.(dir / name))
        |> List.filter is_symlink
    | _ -> []

  let unlink_dont_wait ~on_exn ~sw path =
    Eio.Fiber.fork ~sw (fun () ->
        try Eio.Path.unlink path with err -> on_exn err)

  module Stats = struct
    let is_darwin =
      lazy
        (try
           match Unix.open_process_in "uname" |> input_line with
           | "Darwin" -> true
           | _ -> false
         with Unix.Unix_error _ -> false)

    let get_wtime () =
      (Mtime_clock.now () |> Mtime.to_uint64_ns |> Int64.to_float) /. 1e9

    let get_stime () = Rusage.((get Self).stime)
    let get_utime () = Rusage.((get Self).utime)

    let get_rusage () =
      let Rusage.{ maxrss; minflt; majflt; inblock; oublock; nvcsw; nivcsw; _ }
          =
        Rusage.(get Self)
      in
      let maxrss =
        if Lazy.force is_darwin then Int64.div maxrss 1000L else maxrss
      in
      Irmin_pack_io.Stats_intf.Latest_gc.
        { maxrss; minflt; majflt; inblock; oublock; nvcsw; nivcsw }
  end

  module Clock = Mtime_clock
  module Progress = Progress
end
