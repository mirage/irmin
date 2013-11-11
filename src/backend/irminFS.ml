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

let with_file file fn =
  debug "with_file %s" file;
  mkdir (Filename.dirname file);
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

module type S = sig
  val path: string
end

module type O = sig
  include S
  val file_of_key: string -> string
end

module X (O: O) (K: IrminKey.S) = struct

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
    return (D O.path)

  let mem t key =
    check t >>= fun () ->
    let file = O.file_of_key key in
    return (Sys.file_exists file)

  let read_full_ba fd =
    IrminChannel.read_buffer fd >>= fun buf ->
    return (IrminBuffer.to_ba buf)

  let read_exn t key =
    debug "read_exn %s" (pretty_key key);
    mem t key >>= function
    | true  -> with_file (O.file_of_key key) read_full_ba
    | false -> unknown key

  let read t key =
    debug "read %s" (pretty_key key);
    mem t key >>= function
    | true  -> with_file
                 (O.file_of_key key)
                 (fun fd ->
                    read_full_ba fd >>= fun ba ->
                    IrminBuffer.dump_ba ~msg:"-->" ba;
                    debug "--> read %s" (pretty_value ba);
                    return (Some ba))
    | false -> return_none

  let list t k =
    return [k]

end

module A (S: S) (K: IrminKey.BINARY) = struct

  module O = struct

    let path = S.path / "objects"

    let file_of_key k =
      let key = K.to_hex (K.of_string k) in
      let len = String.length key in
      let pre = String.sub key 0 2 in
      let suf = String.sub key 2 (len - 2) in
      path / pre / suf

  end

  include X(O)(K)

  let add t value =
    debug "add %s" (pretty_value value);
    check t >>= fun () ->
    let key = K.to_string (K.of_ba value) in
    let file = O.file_of_key key in
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

module M (S: S) (K: IrminKey.S) = struct

  module O = struct

    let path = S.path / "heads"

    let file_of_key key =
      path / key

  end

  include X(O)(K)

  let remove t key =
    debug "remove %s" (pretty_key key);
    let file = O.file_of_key key in
    if Sys.file_exists file then
      Unix.unlink file;
    return_unit

  let update t key value =
    debug "update %s %s" (pretty_key key) (pretty_value value);
    check t >>= fun () ->
    remove t key >>= fun () ->
    with_file (O.file_of_key key) (fun fd ->
        let buf = IrminBuffer.of_ba value in
        IrminChannel.write_buffer fd buf
      )

  let list (D root as t) _ =
    check t >>= fun () ->
    basenames (fun x -> x) root

end

let simple path =
  let module S = struct let path = path end in
  let module K = IrminKey.SHA1 in
  let module A = A(S)(K) in
  let module M = M(S)(K) in
  let module Simple = Irmin.Binary
      (K)(IrminValue.Simple)(IrminTag.Simple)
      (A)(A)(A)(M) in
  (module Simple: Irmin.S)
