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

let src = Logs.Src.create "irmin.unix" ~doc:"Irmin Unix bindings"
module Log = (val Logs.src_log src : Logs.LOG)

module type LOCK = sig
  val with_lock: string -> (unit -> 'a Lwt.t) -> 'a Lwt.t
end

module Lock = struct

  module IO = Git_unix.FS.IO

  let is_stale max_age file =
    IO.file_exists file >>= fun exists ->
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
    IO.remove file

  let lock ?(max_age = 2.) ?(sleep = 0.001) file =
    let rec aux i =
      Log.debug (fun f -> f "lock %d" i);
      is_stale max_age file >>= fun is_stale ->
      if is_stale then (
        Log.err (fun f -> f "%s is stale, removing it." file);
        unlock file >>= fun () ->
        aux 1
      ) else
        let create () =
          let pid = Unix.getpid () in
          IO.mkdir (Filename.dirname file) >>= fun () ->
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
    lock file >>= fun () ->
    Lwt.finalize fn (fun () -> unlock file)

end

module IO = struct

  open Lock

  let getcwd = IO.getcwd
  let mkdir = IO.mkdir
  let read_file = IO.read_file
  let rec_files = IO.rec_files
  let file_exists = IO.file_exists

  let lock_file dir file = Filename.concat dir (file ^ ".lock")

  let remove ?(temp_dir=Filename.get_temp_dir_name ()) file =
    let lock_file = lock_file temp_dir file in
    with_lock lock_file (fun () -> IO.remove file)

  let write_file ?(temp_dir=Filename.get_temp_dir_name ()) file v =
    let lock_file = lock_file temp_dir file in
    with_lock lock_file (fun () -> IO.write_file file v)

  let test_and_set ?(temp_dir=Filename.get_temp_dir_name ()) file ~test ~set =
    let lock_file = lock_file temp_dir file in
    with_lock lock_file (fun () ->
        (file_exists file >>= function
          | false -> Lwt.return_none
          | true  -> read_file file >|= fun v -> Some v)
        >>= fun v ->
        let equal = match test, v with
          | None  , None   -> true
          | Some x, Some y -> Cstruct.equal x y
          | _ -> false
        in
        if not equal then Lwt.return false
        else
          (match set with
           | None   -> IO.remove file
           | Some v -> IO.write_file ~temp_dir file v)
          >|= fun () ->
          true
      )

end

module Irmin_fs = struct
  let config = Irmin_fs.config
  module AO = Irmin_fs.AO(IO)
  module Link = Irmin_fs.Link(IO)
  module RW = Irmin_fs.RW(IO)
  module Make = Irmin_fs.Make(IO)
  module type Config = Irmin_fs.Config
  module AO_ext = Irmin_fs.AO_ext(IO)
  module RW_ext = Irmin_fs.RW_ext(IO)
  module Make_ext = Irmin_fs.Make_ext(IO)
end

module Irmin_git = struct
  let config = Irmin_git.config
  let head = Irmin_git.head
  let bare = Irmin_git.bare
  let level = Irmin_git.level
  module AO = Irmin_git.AO
  module RW = Irmin_git.RW(Lock)
  module Memory = Irmin_git.Memory(Git_unix.Sync.IO)(Git_unix.Zlib)
  module FS =
    Irmin_git.FS(Git_unix.Sync.IO)(Git_unix.Zlib)(Lock)(Git_unix.FS.IO)
end

module Irmin_http = struct
  let config = Irmin_http.config
  let uri = Irmin_http.uri
  module Make = Irmin_http.Make(Cohttp_lwt_unix.Client)
end

module Irmin_http_server = struct
  module type S = Irmin_http_server.S
  module Make = Irmin_http_server.Make (Cohttp_lwt_unix.Server)
end

let info msg =
  let date = Int64.of_float (Unix.gettimeofday ()) in
  let owner =
    (* XXX: get "git config user.name" *)
    Printf.sprintf "Irmin %s.[%d]" (Unix.gethostname()) (Unix.getpid())
  in
  Irmin.Info.v ~date ~owner msg

let set_listen_dir_hook () =
  Irmin.Private.Watch.set_listen_dir_hook Irmin_watcher.hook
