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

open Lwt

module Log = Log.Make(struct let section = "FS" end)
module IB = Irmin.Private

let (/) = Filename.concat

module type Config = sig
  val file_of_key: string -> string
  val key_of_file: string -> string
end

module type IO = sig
  val getcwd: unit -> string
  val mkdir: string -> unit Lwt.t
  val remove: string -> unit Lwt.t
  val rec_files: string -> string list
  val read_file: string -> Cstruct.t Lwt.t
  val write_file: string -> Cstruct.t -> unit Lwt.t
end

(* ~path *)
let of_path, to_path, _path = IB.Config.univ Tc.string
let path_k = "fs:path"
let path_key t = IB.Config.find t path_k to_path

let config ~path =
  let path = [ path_k, of_path path ] in
  IB.Config.of_dict path

module RO_ext (S: Config) (IO: IO) (K: Irmin.HUM) (V: Tc.S0) = struct
  type key = K.t

  type value = V.t

  module W = IB.Watch.Make(K)(V)

  type t = {
    path: string;
    w: W.t;
    config: Irmin.config;
    task: Irmin.task;
  }

  let task t = t.task
  let config t = t.config

  let create config task =
    let w = W.create () in
    let path = match path_key config with
      | None   -> IO.getcwd ()
      | Some p -> p
    in
    IO.mkdir path >>= fun () ->
    return { path; w; config; task; }

  let file_of_key { path; _ } key =
    path / S.file_of_key (K.to_hum key)

  let mk_value x =
    Tc.read_cstruct (module V) x

  let mem t key =
    let file = file_of_key t key in
    Log.debugf "file=%s" file;
    return (Sys.file_exists file)

  let read_exn t key =
    mem t key >>= function
    | false -> fail Not_found
    | true  -> IO.read_file (file_of_key t key) >>= fun x -> return (mk_value x)

  let read t key =
    mem t key >>= function
    | false -> return_none
    | true  ->
      IO.read_file (file_of_key t key) >>= fun x -> return (Some (mk_value x))

  let list _ k =
    return [k]

  let keys_of_dir t =
    let files = IO.rec_files t.path in
    let files  =
      let p = String.length t.path in
      List.map (fun file ->
          let n = String.length file in
          if n <= p + 1 then "" else String.sub file (p+1) (n - p - 1)
        ) files
    in
    List.map (fun file -> K.of_hum (S.key_of_file file)) files

  let dump t =
    let l = keys_of_dir t in
    Lwt_list.fold_left_s (fun acc x ->
        read t x >>= function
        | None   -> return acc
        | Some v -> return ((x, v) :: acc)
      ) [] l

end

module AO_ext (S: Config) (IO: IO) (K: Irmin.Hash.S) (V: Tc.S0) = struct

  include RO_ext(S)(IO)(K)(V)

  let add t value =
    let value = Tc.write_cstruct (module V) value in
    let key = K.digest value in
    let file = file_of_key t key in
    begin if Sys.file_exists file then
        return_unit
      else
        IO.write_file file value
    end >>= fun () ->
    return key

end

module RW_ext (S: Config) (IO: IO) (K: Irmin.HUM) (V: Tc.S0) = struct

  include RO_ext(S)(IO)(K)(V)

  let create config task =
    let w = W.create () in
    let path = match path_key config with
      | None   -> IO.getcwd ()
      | Some p -> p
    in
    let key_of_file file = Some (K.of_hum (S.key_of_file file)) in
    IO.mkdir path >>= fun () ->
    let t = { path; w; config; task; } in
    W.listen_dir w path ~key:key_of_file ~value:(read t);
    return t

  let remove t key =
    let file = file_of_key t key in
    IO.remove file

  let update t key value =
    remove t key >>= fun () ->
    IO.write_file (file_of_key t key) (Tc.write_cstruct (module V) value)
    >>= fun () ->
    W.notify t.w key (Some value);
    return_unit

  let remove t key =
    remove t key >>= fun () ->
    W.notify t.w key None;
    return_unit

  let watch t key =
    IB.Watch.lwt_stream_lift (
      read t key >>= fun value ->
      return (W.watch t.w key value)
    )

end

module Make_ext (Obj: Config) (Ref: Config) (IO: IO)
    (P: Ir_path.S)
    (C: Ir_contents.S)
    (T: Ir_tag.S)
    (H: Ir_hash.S)
= struct
  module AO = AO_ext(Obj)(IO)
  module RW = RW_ext(Ref)(IO)
  include Irmin.Make(AO)(RW)(P)(C)(T)(H)
end


let string_chop_prefix ~prefix str =
  let len = String.length prefix in
  if String.length str <= len then ""
  else String.sub str len (String.length str - len)

module Ref = struct
  let file_of_key key = "refs" / key
  let key_of_file file =
    string_chop_prefix ~prefix:("refs" / "") file
end

module Obj = struct

  let file_of_key k =
    let len = String.length k in
    let pre = String.sub k 0 2 in
    let suf = String.sub k 2 (len - 2) in
    "objects" / pre / suf

  let key_of_file path =
    Log.debugf "key_of_file %s" path;
    let path = string_chop_prefix ~prefix:("objects" / "") path in
    let path = Irmin.Path.String.of_hum path in
    let path = String.concat "" path in
    path

end

module AO = AO_ext (Obj)
module RW = RW_ext (Ref)
module Make = Make_ext(Obj)(Ref)
