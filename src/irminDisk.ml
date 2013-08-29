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

open IrminTypes
open IrminMisc

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

module type S = sig
  type t
  val create: string -> t
  val init: string -> unit Lwt.t
  module Key_store  : KEY_STORE   with type t = t
  module Value_store: sig
    include VALUE_STORE with type t = t
    val dump: t -> unit Lwt.t
  end
  module Tag_store  : TAG_STORE   with type t = t
end

module Disk (K: KEY) (V: VALUE with module Key = K) (T: TAG) = struct

  let debug fmt = IrminMisc.debug "DISK" fmt

  module XKey = IrminIO.File(K)
  module XKeys = IrminIO.File(IrminIO.List(K))
  module XValue = IrminIO.File(V)

  let (/) = Filename.concat

  let keys_dir dir = dir / "keys"

  let key_file dir k = keys_dir dir / K.to_hex k

  let all_keys dir = keys_dir dir / "index"

  let values_dir dir = dir / "values"

  let value_file dir k = values_dir dir / K.to_hex k

  let tags_dir dir = dir / "tags"

  let tag_file dir t = tags_dir dir / T.to_name t

  type file = string

  type files = {
    root : string;
    key  : K.t -> file;
    value: K.t -> file;
    tag  : T.t -> file;
  }

  type t = files

  let check t =
    if not (Sys.file_exists t.root) then
      error "Not an Irminsule repository: %s/" t.root
    else if not (Sys.is_directory t.root) then
      error "%s is not a directory!" t.root
    else
      Lwt.return ()

  let with_file file fn =
    debug "with_file %s" file;
    lwt fd = Lwt_unix.(openfile file [O_RDWR; O_NONBLOCK; O_CREAT] 0o644) in
    let t = IrminIO.Lwt_channel.create fd file in
    try
      lwt r = fn t in
      lwt () = IrminIO.Lwt_channel.close t in
      Lwt.return r
    with e ->
      lwt () = IrminIO.Lwt_channel.close t in
      raise_lwt e

  let with_maybe_file file fn default =
    if Sys.file_exists file then
      with_file file fn
    else (
      debug "with_maybe_file: %s does not exist, skipping" file;
      Lwt.return default
    )

  let init dir =
    if Sys.file_exists dir then
      warning "Reinitialized existing Irminsule repository in %s/" dir;
    let mkdir dir =
      if not (Sys.file_exists dir) then Unix.mkdir dir 0o755 in
    mkdir dir;
    mkdir (keys_dir dir);
    mkdir (values_dir dir);
    mkdir (tags_dir dir);
    Lwt.return ()

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
        Lwt.return (fn b)
      ) files

  module Key_store = struct

    let debug fmt = IrminMisc.debug "DISK-KEY" fmt

    module Key = K

    type t = files

    let pred t key =
      debug "pred %s" (K.pretty key);
      lwt () = check t in
      lwt keys = with_maybe_file (t.key key) XKeys.read_fd [] in
      Lwt.return (Key.Set.of_list keys)

    let all t =
      debug "all";
      lwt () = check t in
      lwt keys = with_maybe_file (all_keys t.root)  XKeys.read_fd [] in
      Lwt.return (Key.Set.of_list keys)

    let update_index t keys =
      lwt old_keys = all t in
      let keys = Key.Set.union old_keys keys in
      let keys = Key.Set.to_list keys in
      with_file (all_keys t.root) (fun fd -> XKeys.write_fd fd keys)

    let add t key pred_keys =
      debug "add %s %s" (K.pretty key) (K.Set.pretty pred_keys);
      lwt () = check t in
      let file = t.key key in
      lwt old_keys = with_maybe_file file XKeys.read_fd [] in
      let keys = Key.Set.union pred_keys (Key.Set.of_list old_keys) in
      if Sys.file_exists file then Sys.remove file;
      lwt () =
        if Key.Set.is_empty keys then Lwt.return ()
        else with_file file (fun fd ->
            XKeys.write_fd fd (Key.Set.to_list keys)
          ) in
      let keys = Key.Set.union (Key.Set.singleton key) pred_keys in
      update_index t keys

  end

  module Value_store = struct

    let debug fmt = IrminMisc.debug "DISK-VALUE" fmt

    module Key = K

    module Value = V

    type t = files

    let write t value =
      debug "write %s" (V.pretty value);
      lwt () = check t in
      let key = V.key value in
      let file = t.value key in
      if Sys.file_exists file then
        Lwt.return key
      else
        let parents = V.parents value in
        lwt () = Key_store.add t key parents in
        lwt () = with_file file (fun fd ->
            XValue.write_fd fd value
          ) in
        Lwt.return key

    let read t key =
      debug "read %s" (K.pretty key);
      lwt () = check t in
      let file = t.value key in
      if Sys.file_exists file then
        lwt value = with_file file XValue.read_fd in
        Lwt.return (Some value)
      else
        Lwt.return None

    let dump t =
      lwt keys = basenames K.of_hex (values_dir t.root) in
      lwt values = Lwt_list.map_s (fun key ->
          lwt value = with_file (t.value key) XValue.read_fd in
          Lwt.return (key, value)
        ) keys in
      List.iter (fun (key, value) ->
          Printf.printf "%s %s\n" (Key.pretty key) (Value.pretty value)
        ) values;
      Lwt.return ()

  end

  module Tag_store = struct

    let debug fmt = IrminMisc.debug "DISK-TAG" fmt

    module Key = K

    module Tag = T

    type t = files

    let remove t tag =
      lwt () = check t in
      let file = t.tag tag in
      if Sys.file_exists file then (
        debug "remove %s" (T.to_name tag);
        Unix.unlink file;
      );
      Lwt.return ()

    let update t tag keys =
      debug "update %s %s" (Tag.to_name tag) (Key.Set.pretty keys);
      lwt () = check t in
      lwt () = remove t tag in
      with_file (t.tag tag) (fun fd ->
          debug "add %s" (Tag.to_name tag);
          XKeys.write_fd fd (Key.Set.to_list keys)
        )

    let read t tag =
      debug "read %s" (Tag.to_name tag);
      lwt () = check t in
      let file = t.tag tag in
      if Sys.file_exists file then
        lwt keys = with_file file XKeys.read_fd in
        Lwt.return (Key.Set.of_list keys)
      else
        Lwt.return Key.Set.empty

    let all t =
      debug "all";
      lwt () = check t in
      lwt tags = basenames Tag.of_name (tags_dir t.root) in
      Lwt.return (Tag.Set.of_list tags)

  end

end
