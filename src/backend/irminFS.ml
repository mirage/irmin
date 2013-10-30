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

open Lwt

let debug fmt = IrminLog.debug "FS" fmt

exception Error of string

let error fmt =
  Printf.kprintf (fun str ->
      Printf.eprintf "fatal: %s\n%!" str;
      fail (Error str)
    ) fmt

let warning fmt =
  Printf.kprintf (fun str ->
      Printf.eprintf "%s\n%!" str
    ) fmt

let (/) = Filename.concat

let basenames fn dir =
  let files = Lwt_unix.files_of_directory dir in
  Lwt_stream.to_list files >>= fun files ->
  let files = List.filter (fun f -> f <> "." && f <> "..") files in
  Lwt_list.map_s (fun f ->
      let b = Filename.basename f in
      return (fn b)
    ) files

type root = D of string

let check (D root) =
  if not (Sys.file_exists root) then
    error "Not an Irminsule repository: %s/" root
  else if not (Sys.is_directory root) then
    error "%s is not a directory!" root
  else
    return_unit

let safe_mkdir dir =
  if not (Sys.file_exists dir) then
    try Unix.mkdir dir 0o755
    with Unix.Unix_error(Unix.EEXIST,_,_) -> ()

let with_file file fn =
  debug "with_file %s" file;
  safe_mkdir (Filename.dirname file);
  Lwt_unix.(openfile file [O_RDWR; O_NONBLOCK; O_CREAT] 0o644) >>= fun fd ->
  let t = IrminChannel.create fd file in
  try
    let fd = Unix.openfile file [Unix.O_RDWR; Unix.O_NONBLOCK; Unix.O_CREAT] 0o644 in
    let ba = Lwt_bytes.map_file ~fd ~shared:false () in
    IrminBuffer.dump ~msg:"-->" (IrminBuffer.of_ba ba);
    fn t >>= fun r ->
    IrminChannel.close t >>= fun () ->
    return r
  with e ->
    IrminChannel.close t >>= fun () ->
    fail e

let with_maybe_file file fn default =
  if Sys.file_exists file then
    with_file file fn
  else (
    debug "with_maybe_file: %s does not exist, skipping" file;
    return default
  )

let init dir (D root as t) =
  let mkdir dir =
    if not (Sys.file_exists dir) then Unix.mkdir dir 0o755 in
  mkdir root;
  mkdir (dir t);
  return_unit

module Make (R: sig val root: string end) = struct

  module A (K: IrminKey.S) = struct

    let debug fmt = IrminLog.debug "FS-APPEND" fmt

    exception Unknown of K.t

    type t = root

    type key = K.t

    let objects (D root) =
      root / "objects"

    let file_of_key t k =
      let key = K.to_hex k in
      let len = String.length key in
      let pre = String.sub key 0 2 in
      let suf = String.sub key 2 (len - 2) in
      objects t / pre / suf

    let keys_of_dir dir: K.t list Lwt.t =
      let pre = Filename.basename dir in
      basenames (fun suf ->
          K.create (pre ^ suf)
        ) dir

    let create () =
      return (D R.root)

    let init =
      init objects

    let mem t key =
      check t >>= fun () ->
      let file = file_of_key t key in
      return (Sys.file_exists file)

    let read_full_ba fd =
      IrminChannel.read_buffer fd >>= fun buf ->
      return (IrminBuffer.to_ba buf)

    let read_exn t key =
      debug "read_exn %s" (K.pretty key);
      mem t key >>= function
      | true  -> with_file (file_of_key t key) read_full_ba
      | false -> fail (Unknown key)

    let read t key =
      debug "read %s" (K.pretty key);
      mem t key >>= function
      | true  -> with_file
                   (file_of_key t key)
                   (fun fd ->
                      read_full_ba fd >>= fun ba ->
                      IrminBuffer.dump_ba ~msg:"-->" ba;
                      return (Some ba))
      | false -> return_none

    let add t value =
      debug "add"; IrminBuffer.dump_ba value;
      check t >>= fun () ->
      let key = K.of_ba value in
      let file = file_of_key t key in
      begin if Sys.file_exists file then
          return_unit
        else
          let value = IrminBuffer.of_ba value in
          with_file file (fun fd -> IrminChannel.write_buffer fd value)
      end >>= fun () ->
      return key

    let list t k =
      return [k]

    let dump t =
      basenames keys_of_dir (objects t)
      >>= fun keys ->
      Lwt_list.fold_left_s (fun acc keys ->
          keys >>= fun keys ->
          Lwt_list.map_s (fun key ->
              let file = file_of_key t key in
              with_file file IrminChannel.read_buffer
              >>= fun value ->
              let value = IrminBuffer.to_ba value in
              return (key, value)
            ) keys
          >>= fun keys ->
          return (keys @ acc)
        ) [] keys
      >>= fun values ->
      List.iter (fun (key, value) ->
          Printf.eprintf "%s\n" (K.pretty key);
          IrminBuffer.dump_ba value
        ) values;
      return_unit

  end

  module M (K: IrminKey.S) = struct

    let debug fmt = IrminLog.debug "FS-MUTABLE" fmt

    exception Unknown of string

    type key = string

    type value = K.t

    type t = root

    let heads (D root) =
      root / "heads"

    let head t tag =
      heads t / tag

    let create () =
      return (D R.root)

    let init =
      init heads

    let remove t tag =
      check t >>= fun () ->
      let file = head t tag in
      if Sys.file_exists file then (
        debug "remove %s" tag;
        Unix.unlink file;
      );
      return_unit

    let update t tag key =
      debug "update %s %s" tag (K.pretty key);
      check t >>= fun () ->
      remove t tag >>= fun () ->
      with_file (head t tag) (fun fd ->
          debug "add %s %s" tag (K.to_hex key);
          IrminChannel.write_string fd (K.dump key)
        )

    let mem t tag =
      check t >>= fun () ->
      let file = head t tag in
      return (Sys.file_exists file)

    let read_key fd =
      IrminChannel.read_string fd K.length >>= fun str ->
      let key = K.create str in
      debug "read_key: %S %s" str (K.pretty key);
      return key

    let read_exn t tag =
      debug "read_exn %s" tag;
      let file = head t tag in
      mem t tag >>= function
      | true  -> with_file file read_key
      | false -> fail (Unknown tag)

    let read t tag =
      debug "read %s" tag;
      let file = head t tag in
      mem t tag >>= function
      | true  -> with_file file (fun fd -> read_key fd >>= fun k -> return (Some k))
      | false -> return_none

    let list t _ =
      debug "list";
      check t >>= fun () ->
      basenames (fun x -> x) (heads t)

  end

end

let simple root =
  let module S = Make(struct let root = root end) in
  let module SimpleA = S.A(IrminKey.SHA1) in
  let module SimpleM = S.M(IrminKey.SHA1) in
  let module Simple = Irmin.Make
      (IrminKey.SHA1)(IrminValue.Simple)(IrminTag.Simple)
      (SimpleA)(SimpleA)(SimpleA)(SimpleM) in
  (module Simple: Irmin.S)
