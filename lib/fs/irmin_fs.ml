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

open IrminCore
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

module type Config' = sig
  val path: string
  val file_of_key: string -> string
  val key_of_file: string -> string
end

module type IO = sig
  val check_dir: string -> unit Lwt.t
  val with_file_in: string -> (Cstruct.t -> 'a Lwt.t) -> 'a Lwt.t
  val rec_files: string -> string list
  val with_file_out: string -> Cstruct.t -> unit Lwt.t
  val remove_file: string -> unit Lwt.t
end

module RO (IO: IO) (S: Config') (K: IrminKey.S) = struct

  type key = string

  type value = Cstruct.buffer

  module W = IrminWatch.Make(K)(Cstruct)

  type t = {
    t: string;
    w: W.t;
  }

  let pretty_key k =
    K.to_raw (K.of_raw k)

  let unknown k =
    fail (IrminKey.Unknown (pretty_key k))

  let create () =
    let t = S.path in
    let w = W.create () in
    return { t; w }

  let mem { t } key =
    IO.check_dir t >>= fun () ->
    let file = S.file_of_key key in
    Log.debugf "file=%s" file;
    return (Sys.file_exists file)

  let read_exn t key =
    Log.debugf "read_exn %s" (pretty_key key);
    mem t key >>= function
    | true  -> IO.with_file_in (S.file_of_key key) (fun x -> return x)
    | false -> unknown key

  let read t key =
    Log.debugf "read %s" (pretty_key key);
    mem t key >>= function
    | true  -> IO.with_file_in (S.file_of_key key) (fun x -> return (Some x))
    | false -> return_none

  let list { t } k =
    return k

  let keys_of_dir root =
    let files = IO.rec_files root in
    List.map ~f:S.key_of_file files

  let dump ({ t = root } as t) =
    Log.debugf "dump %s" root;
    IO.check_dir t.t >>= fun () ->
    let l = keys_of_dir root in
    Lwt_list.fold_left_s (fun acc x ->
        read t x >>= function
        | None   -> return acc
        | Some v -> return ((x, v) :: acc)
      ) [] l

end

module AO (IO: IO) (S: Config') (K: IrminKey.S) = struct

  include RO(IO)(S)(K)

  let add { t } value =
    Log.debugf "add";
    IO.check_dir t >>= fun () ->
    let key = K.to_raw (K.compute_from_cstruct value) in
    let file = S.file_of_key key in
    begin if Sys.file_exists file then
        return_unit
      else
        IO.with_file_out file value
    end >>= fun () ->
    return key

end

module RW (IO: IO) (S: Config') (K: IrminKey.S) = struct

  include RO(IO)(S)(K)

  let read_key t key =
    read t (K.to_raw key)

  let create () =
    let key_of_file file =
      Some (K.of_string (S.key_of_file file)) in
    let t = S.path in
    let w = W.create () in
    let t = { t; w } in
    W.listen_dir w S.path key_of_file (read_key t);
    return t

  let remove { t } key =
    Log.debugf "remove %s" (pretty_key key);
    let file = S.file_of_key key in
    IO.remove_file file

  let update t key value =
    Log.debugf "update %s" (pretty_key key);
    IO.check_dir t.t >>= fun () ->
    remove t key >>= fun () ->
    IO.with_file_out (S.file_of_key key) value >>= fun () ->
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

module REIO (S: Config) = struct

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

module Make (IO: IO) (X: Config) = struct
  module Y = OBJECTS(X)
  module Z = REIO(X)
  module Make (K: IrminKey.S) (C: IrminContents.S) (T: IrminTag.S) = struct
    include Irmin.Binary(AO(IO)(Y(K))(K))(RW(IO)(Z)(T))(K)(C)(T)
  end
  module RO(K: IrminKey.S) = Irmin.RO_BINARY(RO(IO)(Y(K))(K))(K)
  module AO(K: IrminKey.S) = Irmin.AO_BINARY(AO(IO)(Y(K))(K))(K)
  module RW(K: IrminKey.S) = Irmin.RW_BINARY(RW(IO)(Z)(K))(K)
end

module Make' (IO: IO) (X: Config') = struct
  module Make (K: IrminKey.S) (C: IrminContents.S) (T: IrminTag.S) = struct
    include Irmin.Binary(AO(IO)(X)(K))(RW(IO)(X)(T))(K)(C)(T)
  end
  module RO(K: IrminKey.S) = Irmin.RO_BINARY(RO(IO)(X)(K))(K)
  module AO(K: IrminKey.S) = Irmin.AO_BINARY(AO(IO)(X)(K))(K)
  module RW(K: IrminKey.S) = Irmin.RW_BINARY(RW(IO)(X)(K))(K)
end
