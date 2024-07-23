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

module Conf = Irmin.Backend.Conf
include Irmin.Export_for_backends
open Eio

let src = Logs.Src.create "fs.unix" ~doc:"logs fs unix events"

module Log = (val Logs.src_log src : Logs.LOG)

type fs = Eio.Fs.dir_ty Eio.Path.t
type clock = float Eio.Time.clock_ty Eio.Time.clock
type io = { fs : fs; clock : clock }

let fs_typ : fs Conf.Typ.t = Conf.Typ.create ()
let clock_typ : clock Conf.Typ.t = Conf.Typ.create ()

let spec ~path:fs ~clock =
  let spec = Conf.Spec.v "irmin-fs.unix" in
  let fs = (fs :> fs) in
  let _fs_key =
    let to_string fs = Eio.Path.native_exn fs in
    let of_string str = Ok Eio.Path.(fs / str) in
    let of_json_string str =
      match Irmin.Type.(of_json_string string) str with
      | Ok str -> Ok Eio.Path.(fs / str)
      | Error e -> Error e
    in
    Conf.key' ~typ:fs_typ ~spec ~typename:"_ Eio.Path.t" ~to_string ~of_string
      ~of_json_string "fs" fs
  in
  let clock = (clock :> clock) in
  let _clock_key =
    let to_string _ = "Eio.Time.clock" in
    let of_string _ = Ok clock in
    let of_json_string _ = Ok clock in
    Conf.key' ~typ:clock_typ ~spec ~typename:"_ Eio.Time.clock" ~to_string
      ~of_string ~of_json_string "clock" clock
  in
  spec

let conf ~path ~clock = Conf.empty (spec ~path ~clock)

module IO = struct
  type nonrec io = io

  let io_of_config conf =
    {
      fs = Conf.find_key conf "fs" fs_typ;
      clock = Conf.find_key conf "clock" clock_typ;
    }

  type path = string

  let mkdir_pool = Eio_pool.create 1 (fun () -> ())

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

  let mkdir dirname =
    Eio_pool.use mkdir_pool (fun () ->
        Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 dirname)

  let mkdir_parent file =
    match Eio.Path.split file with
    | None -> ()
    | Some (parent, _) -> mkdir parent

  let file_exists ~io:{ fs; _ } filename = Eio.Path.(is_file (fs / filename))

  module Lock = struct
    let is_stale ~io:{ clock; _ } max_age file =
      try
        let { Eio.File.Stat.mtime; _ } = Eio.Path.stat ~follow:false file in
        if mtime < 1.0 (* ??? *) then false
        else Eio.Time.now clock -. mtime > max_age
      with Eio.Io (Eio.Fs.E (Not_found _), _) -> false

    let unlock file = Path.unlink file

    let lock ~io ?(max_age = 10. *. 60. (* 10 minutes *)) ?(sleep = 0.001) file
        =
      let rec aux i =
        [%log.debug "lock %a %d" Eio.Path.pp file i];
        if is_stale ~io max_age file then (
          [%log.err "%a is stale, removing it." Eio.Path.pp file];
          unlock file;
          aux 1)
        else
          let create () =
            let pid = Unix.getpid () in
            mkdir_parent file;
            Switch.run @@ fun sw ->
            let flow = Path.open_out ~sw file ~create:(`Exclusive 0o600) in
            Flow.copy_string (string_of_int pid) flow
          in
          try create () with
          | Eio.Io (Fs.E (Fs.Already_exists _), _) ->
              let backoff =
                1.
                +. Random.float
                     (let i = float i in
                      i *. i)
              in
              Eio.Time.sleep io.clock (sleep *. backoff);
              aux (i + 1)
          | e -> raise e
      in
      aux 1

    let with_lock ~io file fn =
      match file with
      | None -> fn ()
      | Some f ->
          lock ~io f;
          Fun.protect fn ~finally:(fun () -> unlock f)
  end

  (* we use file locking *)
  type lock = Eio.Fs.dir_ty Eio.Path.t

  let lock_file ~io:{ fs; _ } x = Path.(fs / x)

  let list_files kind dir =
    if Eio.Path.is_directory dir then
      let d = Path.read_dir dir in
      let d = List.map (Path.( / ) dir) d in
      let d = List.filter kind d in
      d
    else []

  let directories dir = list_files Eio.Path.is_directory dir
  let files dir = list_files Eio.Path.is_file dir

  let write_string fd b =
    match String.length b with 0 -> () | _len -> Flow.copy_string b fd

  let _delays = Array.init 20 (fun i -> 0.1 *. (float i ** 2.))
  let remove_dir dir = Eio.Path.rmtree dir

  let remove_file ~io ?lock file =
    Lock.with_lock ~io lock (fun () ->
        let file = Path.(io.fs / file) in
        if Path.is_directory file then remove_dir file
        else
          try Path.unlink file
          with Eio.Io (Eio.Fs.E (Fs.Not_found _), _) -> ())

  let temp_file ~temp_dir file suffix =
    let basename =
      match Eio.Path.split file with
      | None -> "tmp"
      | Some (_, basename) -> basename
    in
    let rec go i =
      let tmp = Eio.Path.(temp_dir / (basename ^ string_of_int i ^ suffix)) in
      if Eio.Path.kind ~follow:false tmp = `Not_found then tmp else go (i + 1)
    in
    go 0

  let with_write_file ~temp_dir file fn =
    mkdir temp_dir;
    mkdir_parent file;
    let tmp_file = temp_file ~temp_dir file "write" in
    Eio_pool.use openfile_pool (fun () ->
        [%log.debug "Writing %a (%a)" Eio.Path.pp file Eio.Path.pp tmp_file];
        Path.(with_open_out ~create:(`Or_truncate 0o644) tmp_file fn);
        Path.rename tmp_file file)

  let read_file_with_read file size =
    let buf = Cstruct.create size in
    Path.with_open_in file @@ fun flow ->
    Flow.read_exact flow buf;
    Cstruct.to_string buf

  let read_file ~io:{ fs; _ } file =
    try
      let file = Path.(fs / file) in
      Eio_pool.use openfile_pool (fun () ->
          [%log.debug "Reading %a" Eio.Path.pp file];
          let { Eio.File.Stat.size; _ } = Eio.Path.stat ~follow:false file in
          let size = Optint.Int63.to_int size in
          let buf = read_file_with_read file size in
          Some buf)
    with Eio.Io _ -> None

  let write_file ~io ~temp_dir ?(lock : lock option) file b =
    let file = Path.(io.fs / file) in
    let temp_dir = Path.(io.fs / temp_dir) in
    let write () =
      with_write_file file ~temp_dir (fun fd -> write_string fd b)
    in
    Lock.with_lock ~io lock (fun () ->
        if Path.is_directory file then remove_dir file;
        write ())

  let test_and_set_file ~io ~temp_dir ~lock file ~test ~set =
    Lock.with_lock ~io (Some lock) (fun () ->
        let v = read_file ~io file in
        let equal =
          match (test, v) with
          | None, None -> true
          | Some x, Some y -> String.equal x y
          | _ -> false
        in
        if not equal then false
        else
          let () =
            match set with
            | None -> remove_file ~io file
            | Some v -> write_file ~io ~temp_dir file v
          in
          true)

  let rec_files ~io:{ fs; _ } dir : path list =
    let dir = Path.(fs / dir) in
    let rec aux accu dir =
      let ds = directories dir in
      let fs = files dir in
      List.fold_left aux (fs @ accu) ds
    in
    aux [] dir |> List.map snd

  let mkdir ~io:{ fs; _ } dirname = mkdir Path.(fs / dirname)
end

module Append_only (K : Irmin.Type.S) (V : Irmin.Type.S) =
  Irmin_fs.Append_only (IO) (K) (V)

module Atomic_write = Irmin_fs.Atomic_write (IO)
include Irmin_fs.Maker (IO)
module KV = Irmin_fs.KV (IO)
module Append_only_ext = Irmin_fs.Append_only_ext (IO)
module Atomic_write_ext = Irmin_fs.Atomic_write_ext (IO)
module Maker_ext = Irmin_fs.Maker_ext (IO)
include Irmin_unix
