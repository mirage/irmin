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

let (/) = Filename.concat

module type Config = sig
  val dir: string -> string
  val file_of_key: string -> string
  val key_of_file: string -> string
end

module type IO = sig
  val getcwd: unit -> string Lwt.t
  val mkdir: string -> unit Lwt.t
  val remove: string -> unit Lwt.t
  val rec_files: string -> string list Lwt.t
  val read_file: string -> Cstruct.t Lwt.t
  val write_file: string -> Cstruct.t -> unit Lwt.t
end

(* ~path *)
let root_key = Irmin.Private.Conf.root

let config ?root () =
  Irmin.Private.Conf.singleton root_key root

module RO_ext (IO: IO) (S: Config) (K: Irmin.Hum.S) (V: Tc.S0) = struct

  type key = K.t

  type value = V.t

  module W = Irmin.Private.Watch.Make(K)(V)

  type t = {
    path: string;
    w: W.t;
    task: Irmin.task;
  }

  let task t = t.task

  let create config task =
    let w = W.create () in
    let path = match Irmin.Private.Conf.get config root_key with
      | None   -> IO.getcwd ()
      | Some p -> return p
    in
    path >>= fun path ->
    IO.mkdir path >>= fun () ->
    return (fun a -> { path; w; task = task a })

  let file_of_key { path; _ } key =
    path / S.file_of_key (K.to_hum key)

  let mk_value x =
    Tc.read_cstruct (module V) x

  let mem t key =
    let file = file_of_key t key in
    Log.debug "file=%s" file;
    return (Sys.file_exists file)

  let read_exn t key =
    mem t key >>= function
    | false -> fail Not_found
    | true  -> IO.read_file (file_of_key t key) >>= fun x -> return (mk_value x)

  let read t key =
    Log.debug "read";
    mem t key >>= function
    | false -> return_none
    | true  ->
      IO.read_file (file_of_key t key) >>= fun x -> return (Some (mk_value x))

  let keys_of_dir t fn =
    Log.debug "keys_of_dir";
    IO.rec_files (S.dir t.path) >>= fun files ->
    let files  =
      let p = String.length t.path in
      List.map (fun file ->
          let n = String.length file in
          if n <= p + 1 then "" else String.sub file (p+1) (n - p - 1)
        ) files
    in
    Lwt_list.iter_p (fun file ->
        let k = K.of_hum (S.key_of_file file) in
        fn k
      ) files

  let iter t fn =
    keys_of_dir t (fun k -> fn k)

end

module AO_ext (IO: IO) (S: Config) (K: Irmin.Hash.S) (V: Tc.S0) = struct

  include RO_ext(IO)(S)(K)(V)

  let add t value =
    Log.debug "add";
    let value = Tc.write_cstruct (module V) value in
    let key = K.digest value in
    let file = file_of_key t key in
    begin
      if Sys.file_exists file then return_unit
      else catch (fun () -> IO.write_file file value) (fun e -> Log.debug "XXX"; fail e)
    end >>= fun () ->
    return key

end

module RW_ext (IO: IO) (S: Config) (K: Irmin.Hum.S) (V: Tc.S0) = struct

  include RO_ext(IO)(S)(K)(V)

  let key_of_file file = Some (K.of_hum (S.key_of_file file))

  let create config task =
    let w = W.create () in
    let path = match Irmin.Private.Conf.get config root_key with
      | None   -> IO.getcwd ()
      | Some p -> return p
    in
    path >>= fun path ->
    IO.mkdir path >>= fun () ->
    return (fun a -> { path; w; task = task a })

  let remove t key =
    let file = file_of_key t key in
    IO.remove file

  let update t key value =
    Log.debug "update";
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
    W.listen_dir t.w t.path ~key:key_of_file ~value:(read t);
    Irmin.Private.Watch.lwt_stream_lift (
      read t key >>= fun value ->
      return (W.watch t.w key value)
    )

end

module Make_ext (IO: IO) (Obj: Config) (Ref: Config)
    (P: Ir_path.S)
    (C: Ir_contents.S)
    (T: Ir_tag.S)
    (H: Ir_hash.S)
= struct
  module AO = AO_ext(IO)(Obj)
  module RW = RW_ext(IO)(Ref)
  include Irmin.Make(AO)(RW)(P)(C)(T)(H)
end


let string_chop_prefix ~prefix str =
  let len = String.length prefix in
  if String.length str <= len then ""
  else String.sub str len (String.length str - len)

module Ref = struct
  let dir p = p / "refs"
  let file_of_key key = "refs" / key
  let key_of_file file =
    string_chop_prefix ~prefix:("refs" / "") file
end

module Obj = struct

  let dir t = t / "objects"

  let file_of_key k =
    let len = String.length k in
    let pre = String.sub k 0 2 in
    let suf = String.sub k 2 (len - 2) in
    "objects" / pre / suf

  let key_of_file path =
    Log.debug "key_of_file %s" path;
    let path = string_chop_prefix ~prefix:("objects" / "") path in
    let path = Stringext.split ~on:'/' path in
    let path = String.concat "" path in
    path

end

module AO (IO: IO) = AO_ext (IO)(Obj)
module RW (IO: IO) = RW_ext (IO)(Ref)
module Make (IO: IO) = Make_ext (IO)(Obj)(Ref)
