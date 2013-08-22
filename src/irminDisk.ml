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

  module XKey = IrminIO.Channel(K)
  module XKeys = IrminIO.Channel(IrminIO.List(K))
  module XValue = IrminIO.Channel(V)

  exception Error of string

  let error fmt =
    Printf.kprintf (fun str ->
        Printf.eprintf "%s\n!" str;
        raise (Error str)
      ) fmt

  let (/) = Filename.concat

  let pred_keys dir = dir / "pred-keys"

  let pred_key dir k = pred_keys dir / K.to_hex k

  let succ_keys dir = dir / "succ-keys"

  let succ_key dir k = succ_keys dir / K.to_hex k

  let values dir = dir / "objects"

  let value dir k = values dir / K.to_hex k

  let tags dir = dir / "tags"

  let tag dir t = tags dir / T.to_name t

  type file = string

  type files = {
    root : string;
    pred : K.t -> file;
    succ : K.t -> file;
    value: K.t -> file;
    tag  : T.t -> file;
  }

  type t = files

  let with_file (file:string) (fn:Lwt_unix.file_descr -> 'a Lwt.t): 'a Lwt.t =
    debug "Disk.with_file %s" file;
    lwt fd = Lwt_unix.(openfile file [O_RDWR; O_NONBLOCK; O_CREAT] 0o644) in
    try
      lwt r = fn fd in
      lwt () = Lwt_unix.close fd in
      Lwt.return r
    with e ->
      lwt () = Lwt_unix.close fd in
      raise_lwt e

  let init dir =
    if Sys.file_exists dir then
      error "%s already exists!" dir
    else (
      Unix.mkdir dir 0o755;
      Unix.mkdir (values dir) 0o755;
      Unix.mkdir (tags dir) 0o755;
      Unix.mkdir (succ_keys dir) 0o755;
      Unix.mkdir (pred_keys dir) 0o755;
    );
    Lwt.return ()

  let create dir =
    if not (Sys.file_exists dir) then
      error "%s does not exist!" dir
    else if not (Sys.is_directory dir) then
      error "%s is not a directory!" dir
    else
      {
        root  = dir;
        pred  = pred_key dir;
        succ  = succ_key dir;
        value = value dir;
        tag   = tag dir;
      }

  let basenames fn dir =
    let files = Lwt_unix.files_of_directory dir in
    lwt files = Lwt_stream.to_list files in
    Lwt_list.map_s (fun f ->
        let b = Filename.basename f in
        Lwt.return (fn b)
      ) files

  module Key_store = struct

    module Key = K

    type t = files

    let succ t key =
      with_file (t.succ key) XKeys.read_fd

    let pred t key =
      with_file (t.pred key) XKeys.read_fd

    let list t =
      basenames K.of_hex (values t.root)

    let add_key t k =
      let add file =
        if not (Sys.file_exists file) then
          with_file file (fun fd ->
              XKeys.write_fd fd []
            ) else
          Lwt.return () in
      lwt () = add (t.pred k) in
      lwt () = add (t.succ k) in
      Lwt.return ()

    let add_relation t k1 k2 =
      let add file k =
        with_file file (fun fd ->
            lwt keys = XKeys.read_fd fd in
            lwt _ = Lwt_unix.lseek fd 0 Lwt_unix.SEEK_SET in
            XKeys.write_fd fd (k :: keys)
          ) in
      lwt () = add_key t k1 in
      lwt () = add_key t k2 in
      lwt () = add (t.pred k2) k1 in
      lwt () = add (t.succ k1) k2 in
      Lwt.return ()

  end

  module Value_store = struct

    module Key = K

    module Value = V

    type t = files

    let write t value =
      let key = V.key value in
      let file = t.value key in
      if Sys.file_exists file then
        Lwt.return key
      else
        lwt () = Key_store.add_key t key in
        let preds = V.pred value in
        lwt () = Lwt_list.iter_s (fun k -> Key_store.add_relation t k key) preds in
        lwt () = with_file file (fun fd ->
            XValue.write_fd fd value
          ) in
        Lwt.return key

    let read t key =
      let file = t.value key in
      IrminMisc.debug "Value_store.read %s" file;
      if Sys.file_exists file then
        lwt value = with_file file XValue.read_fd in
        Lwt.return (Some value)
      else
        Lwt.return None

    let dump t =
      lwt keys = basenames K.of_hex (values t.root) in
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

    module Key = K

    module Tag = T

    type t = files

    let remove t tag =
      let file = t.tag tag in
      if Sys.file_exists file then
        Unix.unlink file;
      Lwt.return ()

    let update t tag key =
      lwt () = remove t tag in
      with_file (t.tag tag) (fun fd ->
          XKey.write_fd fd key
        )

    let read t tag =
      let file = t.tag tag in
      if Sys.file_exists file then
        lwt key = with_file file XKey.read_fd in
        Lwt.return (Some key)
      else
        Lwt.return None

    let list t =
      basenames T.of_name (tags t.root)

  end

end
