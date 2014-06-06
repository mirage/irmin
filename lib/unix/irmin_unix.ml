(*
 * Copyright (c) 2013-2014 Thomas Gazagnaire <thomas@gazagnaire.org>
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

open Core_kernel.Std
open Lwt
open Git_unix

module Log = Log.Make(struct let section = "UNIX" end)

let (/) = Filename.concat

exception Error of string

let error fmt =
  Printf.ksprintf (fun str ->
      Printf.eprintf "fatal: %s\n%!" str;
      fail (Error str)
    ) fmt

module IO: IrminFS.IO = struct

  let in_dir dir fn =
    let reset_cwd =
      let cwd =
        try Some (Sys.getcwd ())
        with _ -> None in
      fun () ->
        match cwd with
        | None     -> ()
        | Some cwd -> try Unix.chdir cwd with _ -> () in
    Unix.chdir dir;
    try
      let r = fn () in
      reset_cwd ();
      r
    with e ->
      reset_cwd ();
      raise e

  let list kind dir =
    if Sys.file_exists dir then
      in_dir dir (fun () ->
          let d = Sys.readdir (Sys.getcwd ()) in
          let d = Array.to_list d in
          let l = List.filter ~f:kind d in
          List.sort ~cmp:compare (List.rev_map ~f:(Filename.concat dir) l)
        )
    else
      []

  let directories_with_links =
    list (fun f -> try Sys.is_directory f with _ -> false)

  let files_with_links =
    list (fun f -> try not (Sys.is_directory f) with _ -> true)

  let rec_files root =
    let rec aux accu dir =
      let d = directories_with_links dir in
      let f = files_with_links dir in
      List.fold_left ~f:aux ~init:(f @ accu) d in
    let files = aux [] root in
    let prefix = root / "" in
    List.map ~f:(String.chop_prefix_exn ~prefix) files

  let mkdir dir =
    let safe_mkdir dir =
      if not (Sys.file_exists dir) then
        try Unix.mkdir dir 0o755
        with Unix.Unix_error(Unix.EEXIST,_,_) -> () in
    let rec aux dir =
      if not (Sys.file_exists dir) then begin
        aux (Filename.dirname dir);
        safe_mkdir dir;
      end in
    aux dir

  let check_dir root =
    if Sys.file_exists root && not (Sys.is_directory root) then
      error "%s is not a directory!" root
    else begin
      let mkdir dir =
        if not (Sys.file_exists dir) then mkdir dir in
      mkdir root;
      return_unit
    end

  let files = Lwt_pool.create 50 (fun () -> return_unit)

  let with_file fn =
    Lwt_pool.use files fn

  let read_bigstring fd =
    Lwt_bytes.map_file ~fd ~shared:false ()

  let with_file_in file fn =
    Log.debugf "with_file_in %s" file;
    with_file (fun () ->
        let fd = Unix.(openfile file [O_RDONLY; O_NONBLOCK] 0o644) in
        try
          let b = read_bigstring fd in
          fn b >>= fun r ->
          Unix.close fd;
          return r
        with e ->
          Unix.close fd;
          fail e
      )

  let write_bigstring fd ba =
    let rec rwrite fd buf ofs len =
      Log.debugf " ... write_buf %d" len;
      Lwt_bytes.write fd buf ofs len >>= fun n ->
      if n = 0 then fail End_of_file
      else if n < len then rwrite fd buf (ofs + n) (len - n)
      else return () in
    rwrite fd ba 0 (Bigarray.Array1.dim ba)

  let with_file_out file ba =
    Log.debugf "with_file_out %s" file;
    mkdir (Filename.dirname file);
    with_file (fun () ->
        Lwt_unix.(openfile file [O_RDWR; O_NONBLOCK; O_CREAT] 0o644) >>= fun fd ->
        try
          write_bigstring fd ba >>= fun r ->
          Lwt_unix.close fd >>= fun () ->
          return r
        with e ->
          Lwt_unix.close fd >>= fun () ->
          fail e
      )

  let with_maybe_file_in file fn default =
    if Sys.file_exists file then
      with_file_in file fn
    else (
      Log.debugf "with_maybe_file: %s does not exist, skipping" file;
      return default
    )

  let remove_file file =
    if Sys.file_exists file then
      Unix.unlink file;
    return_unit

end

let install_dir_polling_listener delay =

  IrminWatch.set_listen_dir_hook (fun dir fn ->

      let read_files () =
        let new_files = IO.rec_files dir in
        let new_files = List.map ~f:(fun f -> let f = dir / f in f , Digest.file f) new_files in
        String.Map.of_alist_exn new_files in

      let to_string set =
        Sexp.to_string_hum (String.Map.sexp_of_t String.sexp_of_t set) in

      let rec loop files =
        let new_files = read_files () in
        let diff = Map.merge files new_files (fun ~key -> function
            | `Both (s1, s2)     -> if s1 = s2 then None else Some s1
            | `Left s | `Right s -> Some s
          ) in

        if not (Map.is_empty diff) then
          Log.debugf "polling %s: diff:%s" dir (to_string diff);
        Lwt_list.iter_p (fun (f, _) -> fn f) (String.Map.to_alist diff) >>= fun () ->
        Lwt_unix.sleep delay >>= fun () ->
        loop new_files in

      let t () =
        loop (read_files ()) in

      Lwt.async t
    )

module IrminGit = struct

  open Git
  open Git_unix

  module Memory = IrminGit.Make(struct
      let root = None
      module Store = Git.Memory
      module Sync = Sync.Make(Store)
      let bare = true
      let disk = false
    end)

  module Memory' (C: sig val root: string end) = IrminGit.Make(struct
      let root = Some C.root
      module Store = Git.Memory
      module Sync = Sync.Make(Store)
      let bare = true
      let disk = false
    end)

  module type Config = sig
    val root: string option
    val bare: bool
  end

  module FS (C: Config) = IrminGit.Make(struct
      let root = C.root
      module Store = FS
      module Sync = Sync.Make(Store)
      let bare = C.bare
      let disk = true
    end)

end

module IrminFS = struct
  module Make  = IrminFS.Make (IO)
  module Make' = IrminFS.Make'(IO)
end

module Server = struct

  include Cohttp_lwt_unix.Server

  let listen t ?timeout uri =
    let address = Uri.host_with_default ~default:"localhost" uri in
    let port = match Uri.port uri with
      | None   -> 8080
      | Some p -> p in
    create ?timeout ~address ~port t

end

module IrminHTTP = struct

  module Make = IrminHTTP.Make (Server)

end
