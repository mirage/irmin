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

exception Error of string

let error fmt =
  Printf.kprintf (fun str ->
      Printf.eprintf "fatal: %s\n%!" str;
      raise_lwt (Error str)
    ) fmt

let warning fmt =
  Printf.kprintf (fun str ->
      Printf.eprintf "%s\n%!" str
    ) fmt

let debug fmt = IrminLog.debug "DISK" fmt

module Store (D: sig val dir: string end) = struct

  let (/) = Filename.concat

  let objects = D.dir / "object"

  let file k =
    let key = IrminMisc.hex_encode k in
    let len = String.length keys in
    let pre = String.sub key 0 2 in
    let suf = String.sub key 2 (len - 2) in
    objects / pre // suf

  let check () =
    if not (FD.file_exists D.dir) then
      error "Not an Irminsule repository: %s/" D.dir
    else if not (FD.is_directory D.dir) then
      error "%s is not a directory!" D.dir
    else
      return_unit

  let with_file file fn =
    debug "with_file %s" file;
    Lwt_unix.(openfile file [O_RDWR; O_NONBLOCK; O_CREAT] 0o644) >>= fun fd ->
    let t = FD.create fd file in
    try
      fn t >>= fun r ->
      FD.close t >>= fun () ->
      return r
    with e ->
      FD.close t >>= fun () ->
      raise_lwt e

  let with_maybe_file file fn default =
    if FD.file_exists file then
      with_file file fn
    else (
      debug "with_maybe_file: %s does not exist, skipping" file;
      return default
    )

  let init dir =
    if FD.file_exists dir then
      warning "Reinitialized existing Irminsule repository in %s/" dir;
    let mkdir dir =
      if not (FD.file_exists dir) then FD.mkdir dir in
    mkdir dir;
    mkdir (keys_dir dir);
    mkdir (values_dir dir);
    mkdir (tags_dir dir);
    return_unit

  let create dir = {
    root  = dir;
    key   = key_file dir;
    value = value_file dir;
    tag   = tag_file dir;
  }

  let basenames fn dir =
    let files = Lwt_unix.files_of_directory dir in
    Lwt_stream.to_list files >>= fun files ->
    let files = List.filter (fun f -> f <> "." && f <> "..") files in
    Lwt_list.map_s (fun f ->
        let b = Filename.basename f in
        return (fn b)
      ) files

  let dump t =
    basenames Key.of_hex (values_dir t.root)
    >>= fun keys ->
    Lwt_list.map_s (fun key ->
        with_file (t.value key) XValue.read_channel
        >>= fun value ->
        return (key, value)
      ) keys
    >>= fun values ->
    List.iter (fun (key, value) ->
        Printf.printf "%s %s\n" (Key.pretty key) (Value.pretty value)
      ) values;
    return_unit

  let write t value =
    debug "write %s" (Value.pretty value);
    check t >>= fun () ->
    let key = Value.key value in
    let file = t.value key in
    if Sys.file_exists file then
      return key
    else
      with_file file (fun fd ->
          XValue.write_channel fd value
        ) >>= fun () ->
      return key

  let read t key =
    debug "read %s" (Key.pretty key);
    check t >>= fun () ->
    let file = t.value key in
    if Sys.file_exists file then
      with_file file XValue.read_channel
      >>= fun value ->
      return (Some value)
    else
      return_none

end

module Tag_store = struct

  let debug fmt = IrminLog.debug "DISK-TAG" fmt

  include T

  let remove t tag =
    check t >>= fun () ->
    let file = t.tag tag in
    if Sys.file_exists file then (
      debug "remove %s" (Tag.to_string tag);
      Unix.unlink file;
    );
    return_unit

  let update t tag keys =
    debug "update %s %s" (Tag.pretty tag) (KeySet.pretty keys);
    check t >>= fun () ->
    remove t tag >>= fun () ->
    with_file (t.tag tag) (fun fd ->
        debug "add %s" (Tag.to_string tag);
        XKeys.write_channel fd (KeySet.to_list keys)
      )

  let read t tag =
    debug "read %s" (Tag.pretty tag);
    check t >>= fun () ->
    let file = t.tag tag in
    if Sys.file_exists file then
      with_file file XKeys.read_channel
    else
      return []

  let all t =
    debug "all";
    check t >>= fun () ->
    basenames Tag.of_string (tags_dir t.root)

end
