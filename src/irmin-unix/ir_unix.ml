(*
 * Copyright (c) 2013-2017 Thomas Gazagnaire <thomas@gazagnaire.org>
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

module IO = struct

  module Log = struct
    let src = Logs.Src.create "git.unix" ~doc:"logs git's unix events"
    include (val Logs.src_log src : Logs.LOG)
  end

  let mkdir_pool = Lwt_pool.create 1 (fun () -> Lwt.return_unit)

  let mmap_threshold = 4096
  (* Files smaller than this are loaded using [read].  Use of mmap is
     necessary to handle packfiles efficiently. Since these are stored
     in a weak map, we won't run out of open files if we keep
     accessing the same one.  Using read is necessary to handle
     references, since these are mutable and can't be cached. Using
     mmap here leads to hitting the OS limit on the number of open
     files.  This threshold must be larger than the size of a
     reference.  *)

  (* Pool of opened files *)
  let openfile_pool = Lwt_pool.create 200 (fun () -> Lwt.return_unit)

  let protect_unix_exn = function
    | Unix.Unix_error _ as e -> Lwt.fail (Failure (Printexc.to_string e))
    | e -> Lwt.fail e

  let ignore_enoent = function
    | Unix.Unix_error (Unix.ENOENT, _, _) -> Lwt.return_unit
    | e -> Lwt.fail e

  let protect f x = Lwt.catch (fun () -> f x) protect_unix_exn
  let safe f x = Lwt.catch (fun () -> f x) ignore_enoent

  let mkdir dirname =
    let rec aux dir =
      if Sys.file_exists dir && Sys.is_directory dir then Lwt.return_unit
      else (
        let clear =
          if Sys.file_exists dir then (
            Log.debug (fun l ->
                l "%s already exists but is a file, removing." dir);
            safe Lwt_unix.unlink dir
          ) else
            Lwt.return_unit
        in
        clear >>= fun () ->
        aux (Filename.dirname dir) >>= fun () ->
        Log.debug (fun l -> l "mkdir %s" dir);
        protect (Lwt_unix.mkdir dir) 0o755;
      ) in
    Lwt_pool.use mkdir_pool (fun () -> aux dirname)

  let file_exists f =
    Lwt.catch (fun () -> Lwt_unix.file_exists f) (function
        (* See https://github.com/ocsigen/lwt/issues/316 *)
        | Unix.Unix_error (Unix.ENOTDIR, _, _) -> Lwt.return_false
        | e -> Lwt.fail e)

  module Lock = struct

    let is_stale max_age file =
      file_exists file >>= fun exists ->
      if exists then (
        Lwt.catch (fun () ->
            Lwt_unix.stat file >>= fun s ->
            let stale = Unix.gettimeofday () -. s.Unix.st_mtime > max_age in
            Lwt.return stale)
          (function
            | Unix.Unix_error (Unix.ENOENT, _, _) -> Lwt.return false
            | e -> Lwt.fail e)
      ) else
        Lwt.return false

    let unlock file =
      Lwt_unix.unlink file

    let lock ?(max_age = 10. *. 60. (* 10 minutes *)) ?(sleep = 0.001) file =
      let rec aux i =
        Log.debug (fun f -> f "lock %s %d" file i);
        is_stale max_age file >>= fun is_stale ->
        if is_stale then (
          Log.err (fun f -> f "%s is stale, removing it." file);
          unlock file >>= fun () ->
          aux 1
        ) else
          let create () =
            let pid = Unix.getpid () in
            mkdir (Filename.dirname file) >>= fun () ->
            Lwt_unix.openfile file [Unix.O_CREAT; Unix.O_RDWR; Unix.O_EXCL] 0o600
            >>= fun fd ->
            let oc = Lwt_io.of_fd ~mode:Lwt_io.Output fd in
            Lwt_io.write_int oc pid >>= fun () ->
            Lwt_unix.close fd
          in
          Lwt.catch create (function
              | Unix.Unix_error(Unix.EEXIST, _, _) ->
                let backoff = 1. +. Random.float (let i = float i in i *. i) in
                Lwt_unix.sleep (sleep *. backoff) >>= fun () ->
                aux (i+1)
              | e -> Lwt.fail e)
      in
      aux 1

    let with_lock file fn =
      match file with
      | None   -> fn ()
      | Some f -> lock f >>= fun () -> Lwt.finalize fn (fun () -> unlock f)

  end

  type path = string

  (* we use file locking *)
  type lock = path
  let lock_file x = x

  let file_exists = file_exists

  let list_files kind dir =
    if Sys.file_exists dir && Sys.is_directory dir then
      let d = Sys.readdir dir in
      let d = Array.to_list d in
      let d = List.map (Filename.concat dir) d in
      let d = List.filter kind d in
      let d = List.sort String.compare d in
      Lwt.return d
    else
      Lwt.return_nil

  let directories dir =
    list_files (fun f ->
        try Sys.is_directory f with Sys_error _ -> false
      ) dir

  let files dir =
    list_files (fun f ->
        try not (Sys.is_directory f) with Sys_error _ -> false
      ) dir

  let write_cstruct fd b =
    let rec rwrite fd buf ofs len =
      Lwt_bytes.write fd buf ofs len >>= fun n ->
      if len = 0 then Lwt.fail End_of_file
      else if n < len then rwrite fd buf (ofs + n) (len - n)
      else Lwt.return_unit in
    match Cstruct.len b with
    | 0   -> Lwt.return_unit
    | len -> rwrite fd (Cstruct.to_bigarray b) 0 len

  let delays = Array.init 20 (fun i -> 0.1 *. (float i) ** 2.)

  let command fmt =
    Printf.ksprintf (fun str ->
        Log.debug (fun l -> l "[exec] %s" str);
        let i = Sys.command str in
        if i <> 0 then Log.debug (fun l -> l "[exec] error %d" i);
        Lwt.return_unit
      ) fmt

  let remove_dir dir =
    if Sys.os_type = "Win32" then
      command "cmd /d /v:off /c rd /s /q %S" dir
    else
      command "rm -rf %S" dir

  let remove_file ?lock file =
    Lock.with_lock lock (fun () ->
        Lwt.catch
          (fun () -> Lwt_unix.unlink file)
          (function
            (* On Windows, [EACCES] can also occur in an attempt to
               rename a file or directory or to remove an existing
               directory. *)
            | Unix.Unix_error (Unix.EACCES, _, _)
            | Unix.Unix_error (Unix.EISDIR, _, _) -> remove_dir file
            | Unix.Unix_error (Unix.ENOENT, _, _) -> Lwt.return_unit
            | e -> Lwt.fail e)
      )

  let rename =
    if Sys.os_type <> "Win32" then Lwt_unix.rename
    else
      fun tmp file ->
        let rec aux i =
          Lwt.catch
            (fun () -> Lwt_unix.rename tmp file)
            (function
              (* On Windows, [EACCES] can also occur in an attempt to
                 rename a file or directory or to remove an existing
                 directory. *)
              | Unix.Unix_error (Unix.EACCES, _, _) as e ->
                if i >= Array.length delays then Lwt.fail e
                else (
                  file_exists file >>= fun exists ->
                  if exists && Sys.is_directory file then (
                    remove_dir file >>= fun () -> aux (i+1)
                  ) else (
                    Log.debug (fun l ->
                        l "Got EACCES, retrying in %.1fs" delays.(i));
                    Lwt_unix.sleep delays.(i) >>= fun () -> aux (i+1)
                  ))
              | e -> Lwt.fail e)
        in
        aux 0

  let with_write_file ?temp_dir file fn =
    begin match temp_dir with
      | None   -> Lwt.return_unit
      | Some d -> mkdir d
    end >>= fun () ->
    let dir = Filename.dirname file in
    mkdir dir >>= fun () ->
    let tmp = Filename.temp_file ?temp_dir (Filename.basename file) "write" in
    Lwt_pool.use openfile_pool (fun () ->
        Log.debug (fun l -> l "Writing %s (%s)" file tmp);
        Lwt_unix.(openfile tmp [O_WRONLY; O_NONBLOCK; O_CREAT; O_TRUNC] 0o644)
        >>= fun fd ->
        Lwt.finalize (fun () -> protect fn fd) (fun () -> Lwt_unix.close fd)
        >>= fun () ->
        rename tmp file
      )

  let read_file_with_read file size =
    let chunk_size = max 4096 (min size 0x100000) in
    let buf = Cstruct.create size in
    let flags = [Unix.O_RDONLY] in
    let perm = 0o0 in
    Lwt_unix.openfile file flags perm >>= fun fd ->
    let rec aux off =
      let read_size = min chunk_size (size - off) in
      Lwt_bytes.read fd buf.Cstruct.buffer off read_size >>= fun read ->
      (* It should test for read = 0 in case size is larger than the
         real size of the file. This may happen for instance if the
         file was truncated while reading. *)
      let off = off + read in
      if off >= size then
        Lwt.return buf
      else
        aux off
    in
    Lwt.finalize (fun () -> aux 0)
      (fun () -> Lwt_unix.close fd)

  let read_file_with_mmap file =
    let fd = Unix.(openfile file [O_RDONLY; O_NONBLOCK] 0o644) in
    let ba = Lwt_bytes.map_file ~fd ~shared:false () in
    Unix.close fd;
    Lwt.return (Cstruct.of_bigarray ba)

  let read_file file =
    Lwt.catch (fun () ->
        Lwt_pool.use openfile_pool (fun () ->
            Log.debug (fun l -> l "Reading %s" file);
            Lwt_unix.stat file >>= fun stats ->
            let size = stats.Lwt_unix.st_size in
            (if size >= mmap_threshold then read_file_with_mmap file
             else read_file_with_read file size
            ) >|= fun buf ->
            Some buf
          )
      ) (function
        | Unix.Unix_error _ | Sys_error _ -> Lwt.return_none
        | e -> Lwt.fail e)

  let write_file ?temp_dir ?lock file b =
    let write () =
      with_write_file file ?temp_dir (fun fd -> write_cstruct fd b)
    in
    Lock.with_lock lock (fun () ->
        Lwt.catch write (function
            | Unix.Unix_error (Unix.EISDIR, _, _) -> remove_dir file >>= write
            | e -> Lwt.fail e
          )
      )

  let test_and_set_file ?temp_dir ~lock file ~test ~set =
    Lock.with_lock (Some lock) (fun () ->
        read_file file >>= fun v ->
        let equal = match test, v with
          | None  , None   -> true
          | Some x, Some y -> Cstruct.equal x y
          | _ -> false
        in
        if not equal then Lwt.return false
        else
          (match set with
           | None   -> remove_file file
           | Some v -> write_file ?temp_dir file v)
          >|= fun () ->
          true
      )
  let rec_files dir =
    let rec aux accu dir =
      directories dir >>= fun ds ->
      files dir       >>= fun fs ->
      Lwt_list.fold_left_s aux (fs @ accu) ds in
    aux [] dir

end

module FS = struct
  module AO = Irmin_fs.AO(IO)
  module Link = Irmin_fs.Link(IO)
  module RW = Irmin_fs.RW(IO)
  module Make = Irmin_fs.Make(IO)
  module KV = Irmin_fs.KV(IO)
  module AO_ext = Irmin_fs.AO_ext(IO)
  module RW_ext = Irmin_fs.RW_ext(IO)
  module Make_ext = Irmin_fs.Make_ext(IO)
end

module Git = struct

  module Ref = Irmin_git.Ref(Git_unix.Net)
  module Make = Irmin_git.Make(Git_unix.Net)
  module KV = Irmin_git.KV(Git_unix.Net)

  module FS = struct

    module G = struct
      include Git_unix.Store
      let v ?dotgit ?compression ?buffers root =
        let buffer = match buffers with
          | None   -> None
          | Some p -> Some (Lwt_pool.use p)
        in
        v ?dotgit ?compression ?buffer root
    end

    module AO   = Irmin_git.AO(G)
    module RW   = Irmin_git.RW(G)
    module Make = Make(G)
    module Ref  = Ref(G)
    module KV   = KV(G)
  end

  module Mem = struct
    module G = Irmin_git.Mem(Digestif.SHA1)
    module AO   = Irmin_git.AO(G)
    module RW   = Irmin_git.RW(G)
    module Make (H: Digestif.S) = Make(Irmin_git.Mem(H))
    module Ref = Ref(G)
    module KV = KV(G)
  end
end

module Http = struct
  module Make = Irmin_http.Make(Cohttp_lwt_unix.Client)
  module KV (C: Irmin.Contents.S) =
    Make
      (Irmin.Metadata.None)
      (C)
      (Irmin.Path.String_list)
      (Irmin.Branch.String)
      (Irmin.Hash.SHA1)
  module Server = Irmin_http_server.Make (Cohttp_lwt_unix.Server)
end

let info ?author fmt =
  Fmt.kstrf (fun msg () ->
      let date = Int64.of_float (Unix.gettimeofday ()) in
      let author = match author with
        | Some a -> a
        | None   ->
          (* XXX: get "git config user.name" *)
          Printf.sprintf "Irmin %s.[%d]" (Unix.gethostname()) (Unix.getpid())
      in
      Irmin.Info.v ~date ~author msg
    ) fmt

let set_listen_dir_hook () =
  Irmin.Private.Watch.set_listen_dir_hook Irmin_watcher.hook
