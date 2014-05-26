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

module Log = Log.Make(struct let section = "FS" end)

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

let files = Lwt_pool.create 50 (fun () -> return_unit)
let with_file fn =
  Lwt_pool.use files fn

let with_file_in file fn =
  Log.debugf "with_file_in %s" file;
  with_file (fun () ->
      let fd = Unix.(openfile file [O_RDONLY; O_NONBLOCK] 0o644) in
      try
        let r = fn fd in
        Unix.close fd;
        return r
      with e ->
        Unix.close fd;
        fail e
    )

let with_file_out file fn =
  Log.debugf "with_file_out %s" file;
  mkdir (Filename.dirname file);
  with_file (fun () ->
      Lwt_unix.(openfile file [O_RDWR; O_NONBLOCK; O_CREAT] 0o644) >>= fun fd ->
      try
        fn fd >>= fun r ->
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

let write_bigstring fd ba =
  let rec rwrite fd buf ofs len =
    Log.debugf " ... write_buf %d" len;
    Lwt_bytes.write fd buf ofs len >>= fun n ->
    if n = 0 then fail End_of_file
    else if n < len then rwrite fd buf (ofs + n) (len - n)
    else return () in
  rwrite fd ba 0 (Bigarray.Array1.dim ba)

module type Config' = sig
  val path: string
  val file_of_key: string -> string
  val key_of_file: string -> string
end


module RO (S: Config') (K: IrminKey.S) = struct

  type key = string

  type value = Cstruct.buffer

  module W = IrminWatch.Make(K)(Bigstring)

  type t = {
    t: root;
    w: W.t;
  }

  let pretty_key k =
    K.to_string (K.of_raw k)

  let unknown k =
    fail (IrminKey.Unknown (pretty_key k))

  let create () =
    let t = D S.path in
    let w = W.create () in
    { t; w }

  let mem { t } key =
    check t >>= fun () ->
    let file = S.file_of_key key in
    Log.debugf "file=%s" file;
    return (Sys.file_exists file)

  let read_bigstring fd =
    Lwt_bytes.map_file ~fd ~shared:false ()

  let read_exn t key =
    Log.debugf "read_exn %s" (pretty_key key);
    mem t key >>= function
    | true  -> with_file_in (S.file_of_key key) read_bigstring
    | false -> unknown key

  let read t key =
    Log.debugf "read %s" (pretty_key key);
    mem t key >>= function
    | true  ->
      with_file_in (S.file_of_key key) read_bigstring >>= fun ba ->
      return (Some ba)
    | false -> return_none

  let list { t } k =
    return k

  let keys_of_dir root =
    let files = rec_files root in
    List.map ~f:S.key_of_file files

  let dump ({ t = D root } as t) =
    Log.debugf "dump %s" root;
    check t.t >>= fun () ->
    let l = keys_of_dir root in
    Lwt_list.fold_left_s (fun acc x ->
        read t x >>= function
        | None   -> return acc
        | Some v -> return ((x, v) :: acc)
      ) [] l

end

module AO (S: Config') (K: IrminKey.S) = struct

  include RO(S)(K)

  let add { t } value =
    Log.debugf "add";
    check t >>= fun () ->
    let key = K.to_raw (K.of_bytes value) in
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

module RW (S: Config') (K: IrminKey.S) = struct

  include RO(S)(K)

  let read_key t key =
    read t (K.to_raw key)

  let create () =
    let key_of_file file =
      Some (K.of_string (S.key_of_file file)) in
    let t = D S.path in
    let w = W.create () in
    let t = { t; w } in
    W.listen_dir w S.path key_of_file (read_key t);
    t

  let remove { t } key =
    Log.debugf "remove %s" (pretty_key key);
    let file = S.file_of_key key in
    if Sys.file_exists file then
      Unix.unlink file;
    return_unit

  let update t key value =
    Log.debugf "update %s" (pretty_key key);
    check t.t >>= fun () ->
    remove t key >>= fun () ->
    with_file_out (S.file_of_key key) (fun fd ->
        write_bigstring fd value
      ) >>= fun () ->
    W.notify t.w (K.of_raw key) (Some value);
    return_unit

  let remote t key =
    remove t key >>= fun () ->
    W.notify t.w (K.of_raw key) None;
    return_unit

  let watch t key =
    Log.debugf "watch %S" (pretty_key key);
    IrminMisc.lift_stream (
      read t key >>= fun value ->
      return (W.watch t.w (K.of_raw key) value)
    )

end

module type Config = sig
  val path: string
end

module REFS (S: Config) = struct

  let path = S.path / "refs"

  let file_of_key key =
    path / key

  let key_of_file file =
    Log.debugf "key_of_file %s" file;
    file

end

module OBJECTS (S: Config) (K: IrminKey.S) = struct

  let path = S.path / "objects"

  let file_of_key k =
    let key = K.to_string (K.of_raw k) in
    let len = String.length key in
    let pre = String.sub key 0 2 in
    let suf = String.sub key 2 (len - 2) in
    path / pre / suf

  let key_of_file path =
    Log.debugf "key_of_file %s" path;
    let path = IrminPath.of_string path in
    let path = String.concat ~sep:"" path in
    K.to_raw (K.of_string path)

end

module Make (X: Config) = struct
  module Y = OBJECTS(X)
  module Z = REFS(X)
  module Make (K: IrminKey.S) (C: IrminContents.S) (T: IrminTag.S) = struct
    include Irmin.Binary(AO(Y(K))(K))(RW(Z)(T))(K)(C)(T)
  end
  module RO(K: IrminKey.S) = Irmin.RO_BINARY(RO(Y(K))(K))(K)
  module AO(K: IrminKey.S) = Irmin.AO_BINARY(AO(Y(K))(K))(K)
  module RW(K: IrminKey.S) = Irmin.RW_BINARY(RW(Z)(K))(K)
end

module Make' (X: Config') = struct
  module Make (K: IrminKey.S) (C: IrminContents.S) (T: IrminTag.S) = struct
    include Irmin.Binary(AO(X)(K))(RW(X)(T))(K)(C)(T)
  end
  module RO(K: IrminKey.S) = Irmin.RO_BINARY(RO(X)(K))(K)
  module AO(K: IrminKey.S) = Irmin.AO_BINARY(AO(X)(K))(K)
  module RW(K: IrminKey.S) = Irmin.RW_BINARY(RW(X)(K))(K)
end

let install_dir_polling_listener delay =
  IrminWatch.set_listen_dir_hook (fun dir fn ->

      let read_files () =
        let new_files = rec_files dir in
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
