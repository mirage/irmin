(*
 * Copyright (c) 2013-2022 Thomas Gazagnaire <thomas@gazagnaire.org>
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

include Irmin.Export_for_backends
open Eio

let src = Logs.Src.create "fs.unix" ~doc:"logs fs unix events"

module Log = (val Logs.src_log src : Logs.LOG)

module IO = struct
  let mkdir_pool = Eio_pool.create 1 (fun () -> ())
  let mmap_threshold = 4096

  (* Files smaller than this are loaded using [read].  Use of mmap is
     necessary to handle packfiles efficiently. Since these are stored
     in a weak map, we won't run out of open files if we keep
     accessing the same one.  Using read is necessary to handle
     references, since these are mutable and can't be cached. Using
     mmap here leads to hitting the OS limit on the number of open
     files.  This threshold must be larger than the size of a
     reference. *)

  (* Pool of opened files *)
  let openfile_pool = Eio_pool.create 200 (fun () -> ())

  let protect_unix_exn = function
    | Unix.Unix_error _ as e -> raise (Failure (Printexc.to_string e))
    | e -> raise e

  let ignore_enoent = function
    | Unix.Unix_error (Unix.ENOENT, _, _) -> ()
    | e -> raise e

  let protect f x = try f x with exn -> protect_unix_exn exn
  let safe f x = try f x with exn -> ignore_enoent exn

  let mkdir dirname =
    let rec aux ((_, path) as dir) =
      if Sys.file_exists path && Sys.is_directory path then ()
      else begin
        if Sys.file_exists path then (
          [%log.debug "%s already exists but is a file, removing." path];
          safe Path.unlink dir);
        let parent = (fst dir, Filename.dirname @@ snd dir) in
        aux parent;
        [%log.debug "mkdir %s" path];
        protect (Path.mkdir ~perm:0o755) dir 
    end
    in
    (* TODO: Pool *)
    Eio_pool.use mkdir_pool (fun () -> aux dirname)

  let file_exists (_, f) =
    try Sys.file_exists f with 
      (* See https://github.com/ocsigen/lwt/issues/316 *)
      | Unix.Unix_error (Unix.ENOTDIR, _, _) -> false
      | e -> raise e

  module Lock = struct
    let is_stale max_age file =
      try
          let s = Eio_unix.run_in_systhread (fun () -> Unix.stat file) in
          if s.Unix.st_mtime < 1.0 (* ??? *) then false
          else Unix.gettimeofday () -. s.Unix.st_mtime > max_age
    with
          | Unix.Unix_error (Unix.ENOENT, _, _) -> false
          | e -> raise e

    let unlock file = Path.unlink file

    let lock ?(max_age = 10. *. 60. (* 10 minutes *)) ?(sleep = 0.001) ((_, file) as fcap) =
      let rec aux i =
        [%log.debug "lock %s %d" file i];
        let is_stale = is_stale max_age file in
        if is_stale then (
          [%log.err "%s is stale, removing it." file];
          unlock fcap;
          aux 1)
        else
          let create () =
            let pid = Unix.getpid () in
            let parent = (fst fcap, Filename.dirname file) in
            mkdir parent;
            Switch.run @@ fun sw ->
            let flow =
              Path.open_out ~sw fcap
                ~create:(`Exclusive 0o600)
            in
            Flow.copy_string (string_of_int pid) flow
          in
          try create () with
            | Unix.Unix_error (Unix.EEXIST, _, _) ->
                let backoff =
                  1.
                  +. Random.float
                       (let i = float i in
                        i *. i)
                in
                Eio_unix.sleep (sleep *. backoff); aux (i + 1)
            | e -> raise e
      in
      aux 1

    let with_lock file fn =
      match file with
      | None -> fn ()
      | Some f -> lock f; Fun.protect fn ~finally:(fun () -> unlock f)
  end

  type path = Eio.Fs.dir Eio.Path.t

  (* we use file locking *)
  type lock = path

  let lock_file x = x
  let file_exists = file_exists

  let list_files kind ((_, dir) as v) =
    if Sys.file_exists dir && Sys.is_directory dir then
      let d = Path.read_dir v in
      let d = List.sort String.compare d in
      let d = List.map (Path.(/) v) d in
      let d = List.filter kind d in
      d
    else []

  let directories dir =
    list_files (fun (_, f) -> try Sys.is_directory f with Sys_error _ -> false) dir

  let files dir =
    list_files
      (fun (_, f) -> try not (Sys.is_directory f) with Sys_error _ -> false)
      dir

  let write_string fd b =
    match String.length b with 0 -> () | _len -> Flow.copy_string b fd

  let _delays = Array.init 20 (fun i -> 0.1 *. (float i ** 2.))

  let command fmt =
    Printf.ksprintf
      (fun str ->
        [%log.debug "[exec] %s" str];
        let i = Sys.command str in
        if i <> 0 then [%log.debug "[exec] error %d" i])
      fmt

  let remove_dir dir =
    if Sys.os_type = "Win32" then command "cmd /d /v:off /c rd /s /q %S" dir
    else command "rm -rf %S" dir

  let remove_file ?lock ((_, file) as f) =
    Lock.with_lock lock (fun () ->
        try Path.unlink f with
            (* On Windows, [EACCES] can also occur in an attempt to
               rename a file or directory or to remove an existing
               directory. *)
            | Unix.Unix_error (Unix.EACCES, _, _)
            | Unix.Unix_error (Unix.EISDIR, _, _) ->
                remove_dir file
            | Unix.Unix_error (Unix.ENOENT, _, _) | Fs.Not_found _ -> ()
            | e -> raise e)

  let rename tmp file = Path.rename tmp file

  let with_write_file ?temp_dir file fn =
    let () =
      match temp_dir with None -> () | Some d -> mkdir d
    in
    let dir = (fst file, Filename.dirname @@ snd file) in
    mkdir dir;
    let temp_dir_path = Option.get temp_dir in
    let temp_dir = snd temp_dir_path in
    let file_f = snd file in
    let tmp_f = Filename.temp_file ~temp_dir (Filename.basename file_f) "write" in
    let tmp_name = Filename.basename tmp_f in
    Eio_pool.use openfile_pool (fun () ->
        [%log.debug "Writing %s (%s) %s %s" file_f tmp_f (snd temp_dir_path) (snd file)];
        Path.(with_open_out ~create:(`Or_truncate 0o644) (temp_dir_path / tmp_name) fn);
        rename Path.(temp_dir_path / tmp_name) file)

  let read_file_with_read file size =
    (* let chunk_size = max 4096 (min size 0x100000) in *)
    let buf = Cstruct.create size in
    (* let flags = [ Unix.O_RDONLY ] in
    let perm = 0o0 in *)
    (* let* fd = Lwt_unix.openfile file flags perm in *)
    Path.with_open_in file @@ fun flow ->
    try
      Flow.read_exact flow buf;
      Cstruct.to_string buf
    with End_of_file -> Cstruct.to_string buf

  let read_file_with_mmap file =
    let open Bigarray in
    let fd = Unix.(openfile file [ O_RDONLY; O_NONBLOCK ] 0o644) in
    let ba = 
      Unix.map_file fd char c_layout false [| -1 |]
      |> Bigarray.array1_of_genarray
    in
    Unix.close fd;

    (* XXX(samoht): ideally we should not do a copy here. *)
    (Bigstringaf.to_string ba)

  let read_file file =
    let file_f = snd file in
    try
        Eio_pool.use openfile_pool (fun () ->
            [%log.debug "Reading %s" file_f];
            let stats = Unix.stat file_f in
            let size = stats.Unix.st_size in
            let buf =
              if size >= mmap_threshold then read_file_with_mmap file_f
              else read_file_with_read file size
            in
            Some buf)
      with
        | Unix.Unix_error _ | Sys_error _ -> None | e -> raise e

  let write_file ?temp_dir ?lock file b =
    let write () =
      with_write_file file ?temp_dir (fun fd -> write_string fd b)
    in
    Lock.with_lock lock (fun () ->
        try write () with
          | Unix.Unix_error (Unix.EISDIR, _, _) -> remove_dir (snd file); write ()
          | e -> raise e)

  let test_and_set_file ?temp_dir ~lock file ~test ~set =
    Lock.with_lock (Some lock) (fun () ->
        let v = read_file file in
        let equal =
          match (test, v) with
          | None, None -> true
          | Some x, Some y -> x = y (* TODO *)
          | _ -> false
        in
        if not equal then false
        else
          let () =
            match set with
            | None -> remove_file file
            | Some v -> write_file ?temp_dir file v
          in
          true)

  let rec_files dir : Fs.dir Path.t list =
    let rec aux accu dir =
      let ds = directories dir in
      let fs = files dir in
      List.fold_left aux (fs @ accu) ds
    in
    aux [] dir
end

module Append_only = Irmin_fs.Append_only (IO)
module Atomic_write = Irmin_fs.Atomic_write (IO)
include Irmin_fs.Maker (IO)
module KV = Irmin_fs.KV (IO)
module Append_only_ext = Irmin_fs.Append_only_ext (IO)
module Atomic_write_ext = Irmin_fs.Atomic_write_ext (IO)
module Maker_ext = Irmin_fs.Maker_ext (IO)
include Irmin_unix
