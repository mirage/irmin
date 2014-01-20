(*
 * Copyright (c) 2013 Thomas Gazagnaire <thomas@gazagnaire.org>
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

module L = Log.Make(struct let section = "FS" end)

exception Error of string

let error fmt =
  Printf.ksprintf (fun str ->
      Printf.eprintf "fatal: %s\n%!" str;
      fail (Error str)
    ) fmt

let warning fmt =
  Printf.ksprintf (fun str ->
      Printf.eprintf "%s\n%!" str
    ) fmt

let (/) = Filename.concat

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

type root = D of string

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

let check (D root) =
  if Sys.file_exists root && not (Sys.is_directory root) then
    error "%s is not a directory!" root
  else begin
    let mkdir dir =
      if not (Sys.file_exists dir) then mkdir dir in
    mkdir root;
    return_unit
  end

let with_file_in file fn =
  L.debugf "with_file_in %s" file;
  let fd = Unix.(openfile file [O_RDONLY; O_NONBLOCK] 0o644) in
  try
    let r = fn fd in
    Unix.close fd;
    return r
  with e ->
    Unix.close fd;
    fail e

let with_file_out file fn =
  L.debugf "with_file_out %s" file;
  mkdir (Filename.dirname file);
  Lwt_unix.(openfile file [O_RDWR; O_NONBLOCK; O_CREAT] 0o644) >>= fun fd ->
  try
    fn fd >>= fun r ->
    Lwt_unix.close fd >>= fun () ->
    return r
  with e ->
    Lwt_unix.close fd >>= fun () ->
    fail e

let with_maybe_file_in file fn default =
  if Sys.file_exists file then
    with_file_in file fn
  else (
    L.debugf "with_maybe_file: %s does not exist, skipping" file;
    return default
  )

let write_bigstring fd ba =
  let rec rwrite fd buf ofs len =
    L.debugf " ... write_buf %d" len;
    Lwt_bytes.write fd buf ofs len >>= fun n ->
    if n = 0 then fail End_of_file
    else if n < len then rwrite fd buf (ofs + n) (len - n)
    else return () in
  rwrite fd ba 0 (Bigarray.Array1.dim ba)

module type S = sig
  val path: string
  val file_of_key: string -> string
  val keys_of_dir: string -> string list
end

module type S0 = sig
  val path: string
end

module OBJECTS (S: S0) (K: IrminKey.S) = struct

    let path = S.path / "objects"

    let file_of_key k =
      let key = K.pretty (K.of_string k) in
      let len = String.length key in
      let pre = String.sub key 0 2 in
      let suf = String.sub key 2 (len - 2) in
      path / pre / suf

    let keys_of_dir root =
      let files = rec_files root in
      List.map ~f:(fun path ->
          let path = IrminPath.of_string path in
          K.to_string (K.of_pretty (String.concat ~sep:"" path))
        ) files

  end

module REFS (S: S0) = struct

  let path = S.path / "refs"

  let file_of_key key =
    path / key

  let keys_of_dir root =
    Log.debugf "keys_of_dir %s" root;
    let files = rec_files root in
    List.map ~f:(fun x -> x) files

end


module RO (S: S) (K: IrminKey.S) = struct

  type key = string

  type value = Cstruct.buffer

  type t = root

  let pretty_key k =
    K.pretty (K.of_string k)

  let pretty_value ba =
    let b = Buffer.create 1024 in
    Cstruct.hexdump_to_buffer b (Cstruct.of_bigarray ba);
    Printf.sprintf "%S" (Buffer.contents b)

  let unknown k =
    fail (IrminKey.Unknown (K.pretty (K.of_string k)))

  let create () =
    return (D S.path)

  let mem t key =
    check t >>= fun () ->
    let file = S.file_of_key key in
    Log.debugf "file=%s" file;
    return (Sys.file_exists file)

  let read_bigstring fd =
    Lwt_bytes.map_file ~fd ~shared:false ()

  let read_exn t key =
    L.debugf "read_exn %s" (pretty_key key);
    mem t key >>= function
    | true  -> with_file_in (S.file_of_key key) read_bigstring
    | false -> unknown key

  let read t key =
    L.debugf "read %s" (pretty_key key);
    mem t key >>= function
    | true  ->
      with_file_in (S.file_of_key key) read_bigstring >>= fun ba ->
      return (Some ba)
    | false -> return_none

  let list t k =
    return [k]

  let contents (D root as t) =
    L.debugf "contents %s" root;
    check t >>= fun () ->
    let l = S.keys_of_dir root in
    Lwt_list.fold_left_s (fun acc x ->
        read t x >>= function
        | None   -> return acc
        | Some v -> return ((x, v) :: acc)
      ) [] l

end

module AO (S: S) (K: IrminKey.S) = struct

  include RO(S)(K)

  let add t value =
    L.debugf "add %s" (pretty_value value);
    check t >>= fun () ->
    let key = K.to_string (K.of_bigarray value) in
    let file = S.file_of_key key in
    begin if Sys.file_exists file then
        return_unit
      else
        with_file_out file (fun fd ->
            write_bigstring fd value
          )
    end >>= fun () ->
    return key

end

module RW (S: S) (K: IrminKey.S) = struct

  include RO(S)(K)

  let remove t key =
    L.debugf "remove %s" (pretty_key key);
    let file = S.file_of_key key in
    if Sys.file_exists file then
      Unix.unlink file;
    return_unit

  let update t key value =
    L.debugf "update %s %s" (pretty_key key) (pretty_value value);
    check t >>= fun () ->
    remove t key >>= fun () ->
    with_file_out (S.file_of_key key) (fun fd ->
        write_bigstring fd value
      )

end

let simple path =
  let module S = struct let path = path end in
  let module K = IrminKey.SHA1 in
  let module R = IrminReference.Simple in
  let module Obj = OBJECTS(S)(K) in
  let module Ref = REFS(S) in
  let module Simple = Irmin.Simple (AO(Obj)(K))(RW(Ref)(R)) in
  (module Simple: Irmin.SIMPLE)
