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

(** TODO *)
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
  (** TODO *)

  (** TODO *)
  let unix_error_t =
    Irmin.Type.(map string (fun _str -> assert false) Unix.error_message)

  (** TODO *)
  let misc_error_t = Irmin.Type.(triple unix_error_t string string)

  type create_error = [ `Io_misc of misc_error | `File_exists of string ]
  (** TODO *)

  type open_error =
    [ `Io_misc of misc_error
    | `No_such_file_or_directory of string
    | `Not_a_file ]
  (** TODO *)

  type read_error =
    [ `Io_misc of misc_error
    | `Read_out_of_bounds
    | `Closed
    | `Invalid_argument ]
  (** TODO *)

  type write_error = [ `Io_misc of misc_error | `Ro_not_allowed | `Closed ]
  (** TODO *)

  type close_error = [ `Io_misc of misc_error | `Double_close ]
  (** TODO *)

  type mkdir_error =
    [ `Io_misc of misc_error
    | `File_exists of string
    | `No_such_file_or_directory of string
    | `Invalid_parent_directory ]
  (** TODO *)

  (** TODO *)
  let raise_misc_error (x, y, z) = raise (Unix.Unix_error (x, y, z))

  (** TODO *)
  let catch_misc_error f =
    try Ok (f ())
    with Unix.Unix_error (e, s1, s2) -> Error (`Io_misc (e, s1, s2))

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
    match Eio.Path.kind ~follow:false path with
    | `Regular_file -> `File
    | `Directory -> `Directory
    | `Not_found -> `No_such_file_or_directory
    | _ -> `Other

  (** TODO *)
  let readdir p = Eio.Path.read_dir p

  let default_create_perm = 0o644
  (* let default_open_perm = 0o644 *)
  (* CHECK *)

  let default_mkdir_perm = 0o755

  (** TODO *)
  let create ~sw ~path ~overwrite =
    try
      match Eio.Path.kind ~follow:false path with
      | `Not_found ->
          let file =
            RW
              (Eio.Path.open_out ~sw ~create:(`Exclusive default_create_perm)
                 path)
          in
          Ok { file; closed = false; path }
      | `Regular_file -> (
          match overwrite with
          | true ->
              (* The file exists, truncate it and use it. An exception will be
                 triggered if we don't have the permissions *)
              let file =
                RW
                  (Eio.Path.open_out ~sw
                     ~create:(`Or_truncate default_create_perm) path)
              in
              Ok { file; closed = false; path }
          | false -> Error (`File_exists (Eio.Path.native_exn path)))
      | _ -> assert false
    with
    | Unix.Unix_error (e, s1, s2) -> Error (`Io_misc (e, s1, s2)) (* TODO *)
    | Sys_error _ -> assert false

  let open_ ~sw ~path ~readonly =
    match Eio.Path.kind ~follow:false path with
    | `Not_found ->
        Error (`No_such_file_or_directory (Eio.Path.native_exn path))
    | `Regular_file -> (
        match readonly with
        | true -> (
            try
              let file = RO (Eio.Path.open_in ~sw path) in
              Ok { file; closed = false; path }
            with Unix.Unix_error (e, s1, s2) ->
              Error (`Io_misc (e, s1, s2)) (* TODO *))
        | false -> (
            try
              let file = RW (Eio.Path.open_out ~sw ~create:`Never path) in
              Ok { file; closed = false; path }
            with Unix.Unix_error (e, s1, s2) ->
              Error (`Io_misc (e, s1, s2)) (* TODO *)))
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
  (* TODO *)

  (** TODO *)
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
        (* TODO: Index.Stats is not domain-safe
           Index.Stats.add_write len; *)
        ()

  (** TODO *)
  let write_string t ~off s =
    let len = String.length s in
    try Ok (write_exn t ~off ~len s) with
    | Errors.Closed -> Error `Closed
    | Errors.RO_not_allowed -> Error `Ro_not_allowed
    | Unix.Unix_error (e, s1, s2) -> Error (`Io_misc (e, s1, s2))

  (** TODO *)
  let fsync t =
    match (t.closed, t.file) with
    | true, _ -> Error `Closed
    | _, RO _ -> Error `Ro_not_allowed
    | _, RW file -> (
        try
          Eio.File.sync file;
          Ok ()
        with Unix.Unix_error (e, s1, s2) -> Error (`Io_misc (e, s1, s2)))

  (** TODO *)
  let read_exn t ~off ~len buf =
    if len > Bytes.length buf then raise (Errors.Pack_error `Invalid_argument);
    match t.closed with
    | true -> raise Errors.Closed
    | false -> (
        try
          let file = get_file_as_ro t.file in
          Util.really_read file off len buf
        with exn ->
          Printexc.print_backtrace stderr;
          raise exn)
  (* TODO: Index.Stats is not domain-safe
     Index.Stats.add_read nread; *)
  (* if nread <> len then  *)
  (* TODO: vérifier que c'est bon *)
  (* didn't manage to read the desired amount; in this case the interface seems to
     require we return `Read_out_of_bounds FIXME check this, because it is unusual
     - the normal API allows return of a short string *)
  (* raise (Errors.Pack_error `Read_out_of_bounds) *)

  (** TODO *)
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

  (** TODO *)
  let page_size = 4096

  (** TODO *)
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
        (* TODO: Index.Stats is not domain-safe
           Index.Stats.add_read nread; *)
        Buffer.add_subbytes buf (Cstruct.to_bytes ~off:0 ~len:nread cs) 0 nread;
        if nread = len then aux ~off:Int63.(add off (of_int nread)))
    in
    try
      aux ~off:Int63.zero;
      Ok (Buffer.contents buf)
    with Unix.Unix_error (e, s1, s2) -> Error (`Io_misc (e, s1, s2))

  (** TODO *)
  let read_size t =
    match t.closed with
    | true -> Error `Closed
    | false -> (
        try
          let file = get_file_as_ro t.file in
          Ok Eio.File.(stat file).size
        with Unix.Unix_error (e, s1, s2) -> Error (`Io_misc (e, s1, s2)))

  (** TODO *)
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

  let move_file ~src ~dst =
    try
      Eio.Path.rename src dst;
      Ok ()
    with Eio.Io (err, _context) ->
      Error (`Sys_error (Fmt.str "%a" Eio.Exn.pp_err err))

  let copy_file ~src ~dst =
    let stats = Eio.Path.stat ~follow:false src in
    try
      Eio.Path.with_open_in src (fun in_flow ->
          Eio.Path.with_open_out ~create:(`Or_truncate stats.perm) dst
            (fun out_flow -> Eio.Flow.copy in_flow out_flow));
      Ok ()
    with Eio.Io (err, _context) ->
      Error (`Sys_error (Fmt.str "%a" Eio.Exn.pp_err err))

  (** TODO *)
  let mkdir path =
    let dirname, _ = Option.get @@ Eio.Path.split path in
    match
      (Eio.Path.kind ~follow:false dirname, Eio.Path.kind ~follow:false path)
    with
    | `Directory, `Not_found -> (
        try
          Eio.Path.mkdir ~perm:default_mkdir_perm path;
          Ok ()
        with Unix.Unix_error (e, s1, s2) -> Error (`Io_misc (e, s1, s2)))
    | `Directory, _ -> Error (`File_exists (Eio.Path.native_exn path))
    | `Not_found, `Not_found ->
        Error (`No_such_file_or_directory (Eio.Path.native_exn path))
    | _ -> Error `Invalid_parent_directory

  let rmdir path = Eio.Path.rmdir path

  let unlink path =
    try
      Eio.Path.unlink path;
      Ok ()
    with Eio.Io (err, _context) ->
      Error (`Sys_error (Fmt.str "%a" Eio.Exn.pp_err err))

  let unlink_dont_wait ~on_exn ~sw path =
    Eio.Fiber.fork ~sw (fun () ->
        try Eio.Path.unlink path with err -> on_exn err)

  (** TODO *)
  module Stats = struct
    (** TODO *)
    let is_darwin =
      lazy
        (try
           match Unix.open_process_in "uname" |> input_line with
           | "Darwin" -> true
           | _ -> false
         with Unix.Unix_error _ -> false)

    (** TODO *)
    let get_wtime () =
      (Mtime_clock.now () |> Mtime.to_uint64_ns |> Int64.to_float) /. 1e9

    (** TODO *)
    let get_stime () = Rusage.((get Self).stime)

    (** TODO *)
    let get_utime () = Rusage.((get Self).utime)

    (** TODO *)
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
  (** TODO *)

  module Progress = Progress
  (** TODO *)
end
