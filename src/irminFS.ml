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

module Make (FD: IrminChannel.S) (D: sig val dir: string end) = struct

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
    lwt t = FD.of_file file in
    try
      lwt r = fn t in
      lwt () = FD.close t in
      return r
    with e ->
      lwt () = FD.close t in
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
  lwt files = Lwt_stream.to_list files in
  let files = List.filter (fun f -> f <> "." && f <> "..") files in
  Lwt_list.map_s (fun f ->
      let b = Filename.basename f in
      return (fn b)
    ) files

let dump t =
  lwt keys = basenames Key.of_hex (values_dir t.root) in
  lwt values = Lwt_list.map_s (fun key ->
      lwt value = with_file (t.value key) XValue.read_channel in
      return (key, value)
    ) keys in
  List.iter (fun (key, value) ->
      Printf.printf "%s %s\n" (Key.pretty key) (Value.pretty value)
    ) values;
  return_unit

module Key_store = struct

  let debug fmt = IrminLog.debug "DISK-KEY" fmt

  include T

  let pred t key =
    debug "pred %s" (Key.pretty key);
    lwt () = check t in
    with_maybe_file (t.key key) XKeys.read_channel []

  let keys t ?sources ?sinks () =
    debug "keys";
    lwt () = check t in
    lwt keys = with_maybe_file (all_keys t.root)  XKeys.read_channel [] in
    Key.Graph.make keys ?sources ?sinks (pred t)

  let update_index t ks =
    lwt g = keys t () in
    let old_keys = Key.Graph.vertex g in
    let keys = KeySet.(union (of_list old_keys) (of_list ks)) in
    let keys = KeySet.to_list keys in
    with_file (all_keys t.root) (fun fd -> XKeys.write_channel fd keys)

  let add t key pred_keys =
    debug "add %s %s" (Key.pretty key) (XKeys.pretty pred_keys);
    lwt () = check t in
    let file = t.key key in
    lwt old_keys = with_maybe_file file XKeys.read_channel [] in
    let keys = KeySet.(union (of_list pred_keys) (of_list old_keys)) in
    if Sys.file_exists file then Sys.remove file;
    lwt () =
      if KeySet.is_empty keys then return_unit
      else with_file file (fun fd ->
          XKeys.write_channel fd (KeySet.to_list keys)
        ) in
    let keys = key :: pred_keys in
    update_index t keys

end

module Value_store = struct

  let debug fmt = IrminLog.debug "DISK-VALUE" fmt

  include T

  let write t value =
    debug "write %s" (Value.pretty value);
    lwt () = check t in
    let key = Value.key value in
    let file = t.value key in
    if Sys.file_exists file then
      return_unit key
    else
      lwt () = with_file file (fun fd ->
          XValue.write_channel fd value
        ) in
      return_unit key

  let read t key =
    debug "read %s" (Key.pretty key);
    lwt () = check t in
    let file = t.value key in
    if Sys.file_exists file then
      lwt value = with_file file XValue.read_channel in
      return (Some value)
    else
      return_none

end

module Tag_store = struct

  let debug fmt = IrminLog.debug "DISK-TAG" fmt

  include T

  let remove t tag =
    lwt () = check t in
    let file = t.tag tag in
    if Sys.file_exists file then (
      debug "remove %s" (Tag.to_string tag);
      Unix.unlink file;
    );
    return_unit

  let update t tag keys =
    debug "update %s %s" (Tag.pretty tag) (KeySet.pretty keys);
    lwt () = check t in
    lwt () = remove t tag in
    with_file (t.tag tag) (fun fd ->
        debug "add %s" (Tag.to_string tag);
        XKeys.write_channel fd (KeySet.to_list keys)
      )

  let read t tag =
    debug "read %s" (Tag.pretty tag);
    lwt () = check t in
    let file = t.tag tag in
    if Sys.file_exists file then
      lwt keys = with_file file XKeys.read_channel in
      return keys
    else
      return []

  let all t =
    debug "all";
    lwt () = check t in
    lwt tags = basenames Tag.of_string (tags_dir t.root) in
    return tags

end

let create dir =
  let module M = struct
    include Make(IrminStore.Simple)(IrminChannel.Lwt_unix)
    let create () = create dir
  end in
  (module M)
