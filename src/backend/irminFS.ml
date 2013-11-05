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

let init (D root) subdir =
  let mkdir dir =
    if not (Sys.file_exists dir) then Unix.mkdir dir 0o755 in
  mkdir root;
  mkdir (root / subdir);
  return_unit

module type ROOT = sig
  val root: string
end

module Make (R: ROOT) = struct

  module type S = sig
    val subdir: string
    val file_of_key: root -> string -> string
  end

  module X (S: S) (K: IrminKey.S) = struct

    type t = root

    let pretty_key k =
      K.pretty (K.of_string k)

    let pretty_value ba =
      IrminBuffer.pretty_ba ba

    let unknown k =
      fail (K.Unknown (K.of_string k))

    let keys_of_dir dir: K.t list Lwt.t =
      let pre = Filename.basename dir in
      basenames (fun suf ->
          K.of_string (pre ^ suf)
        ) dir

    let create () =
      return (D R.root)

    let init t =
      init t S.subdir

    let mem t key =
      check t >>= fun () ->
      let file = S.file_of_key t key in
      return (Sys.file_exists file)

    let read_full_ba fd =
      IrminChannel.read_buffer fd >>= fun buf ->
      return (IrminBuffer.to_ba buf)

    let read_exn t key =
      debug "read_exn %s" (pretty_key key);
      mem t key >>= function
      | true  -> with_file (S.file_of_key t key) read_full_ba
      | false -> unknown key

    let read t key =
      debug "read %s" (pretty_key key);
      mem t key >>= function
      | true  -> with_file
                   (S.file_of_key t key)
                   (fun fd ->
                      read_full_ba fd >>= fun ba ->
                      IrminBuffer.dump_ba ~msg:"-->" ba;
                      debug "--> read %s" (pretty_value ba);
                      return (Some ba))
      | false -> return_none

    let list t k =
      return [k]

  end

  module A (K: IrminKey.BINARY): IrminStore.A_BINARY = struct

    module S = struct

      let subdir = "objects"

      let objects (D root) =
        root / subdir

      let file_of_key t k =
        let key = K.to_hex (K.of_string k) in
        let len = String.length key in
        let pre = String.sub key 0 2 in
        let suf = String.sub key 2 (len - 2) in
        objects t / pre / suf

    end

    include X(S)(K)

    let add t value =
      debug "add %s" (pretty_value value);
      check t >>= fun () ->
      let key = K.to_string (K.of_ba value) in
      let file = S.file_of_key t key in
      begin if Sys.file_exists file then
          return_unit
        else
          let value = IrminBuffer.of_ba value in
          with_file file (fun fd ->
              IrminChannel.write_buffer fd value
            )
      end >>= fun () ->
      return key

  end

  module M (K: IrminKey.S): IrminStore.M_BINARY = struct

    module S = struct

      let subdir = "heads"

      let heads (D root) =
        root / subdir

      let file_of_key t key =
        heads t / key

    end

    include X(S)(K)

    let remove t key =
      debug "remove %s" (pretty_key key);
      let file = S.file_of_key t key in
      if Sys.file_exists file then
        Unix.unlink file;
      return_unit

    let update t key value =
      debug "update %s %s" (pretty_key key) (pretty_value value);
      check t >>= fun () ->
      remove t key >>= fun () ->
      with_file (S.file_of_key t key) (fun fd ->
          let buf = IrminBuffer.of_ba value in
          IrminChannel.write_buffer fd buf
        )

  end

end

let simple root =
  let module S = Make(struct
      include IrminKey.SHA1
      let root = root
    end) in
  let module K = IrminKey.SHA1 in
  let module A = S.A(K) in
  let module M = S.M(K) in
  let module Simple = Irmin.Binary
      (K)(IrminValue.Simple)(IrminTag.Simple)
      (A)(A)(A)(M) in
  (module Simple: Irmin.S)
