(*
 * Copyright (c) 2013-2021 Thomas Gazagnaire <thomas@gazagnaire.org>
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

open! Import
open Astring

let src = Logs.Src.create "irmin.fs" ~doc:"Irmin disk persistence"

module Log = (val Logs.src_log src : Logs.LOG)

let ( / ) = Filename.concat

module type Config = sig
  val dir : string -> string
  val file_of_key : string -> string
  val key_of_file : string -> string
end

module type IO = sig
  type path = string

  val rec_files : path -> string list Lwt.t
  val file_exists : path -> bool Lwt.t
  val read_file : path -> string option Lwt.t
  val mkdir : path -> unit Lwt.t

  type lock

  val lock_file : string -> lock
  val write_file : ?temp_dir:path -> ?lock:lock -> path -> string -> unit Lwt.t

  val test_and_set_file :
    ?temp_dir:path ->
    lock:lock ->
    string ->
    test:string option ->
    set:string option ->
    bool Lwt.t

  val remove_file : ?lock:lock -> path -> unit Lwt.t
end

(* ~path *)

let root_key = Irmin.Private.Conf.root
let default_config = Irmin.Private.Conf.(v [ k root_key ])

let config ?(config = Irmin.Private.Conf.empty) root =
  let config = Irmin.Private.Conf.union config default_config in
  Irmin.Private.Conf.add config root_key (Some root)

module Read_only_ext
    (IO : IO)
    (S : Config)
    (K : Irmin.Type.S)
    (V : Irmin.Type.S) =
struct
  type key = K.t
  type value = V.t
  type 'a t = { path : string }

  let get_path config =
    match Irmin.Private.Conf.get config root_key with
    | Some r -> r
    | None -> invalid_arg "The `root` config key is missing"

  let v config =
    let path = get_path config in
    IO.mkdir path >|= fun () -> { path }

  let close _ = Lwt.return_unit
  let cast t = (t :> read_write t)
  let batch t f = f (cast t)

  let file_of_key { path; _ } key =
    path / S.file_of_key (Irmin.Type.to_string K.t key)

  let lock_of_key { path; _ } key =
    IO.lock_file (path / "lock" / S.file_of_key (Irmin.Type.to_string K.t key))

  let mem t key =
    let file = file_of_key t key in
    IO.file_exists file

  let of_bin_string = Irmin.Type.(unstage (of_bin_string V.t))

  let value v =
    match of_bin_string v with
    | Ok v -> Some v
    | Error (`Msg e) ->
        Log.err (fun l -> l "Irmin_fs.value %s" e);
        None

  let pp_key = Irmin.Type.pp K.t

  let find t key =
    Log.debug (fun f -> f "find %a" pp_key key);
    IO.read_file (file_of_key t key) >|= function
    | None -> None
    | Some x -> value x

  let list t =
    Log.debug (fun f -> f "list");
    let+ files = IO.rec_files (S.dir t.path) in
    let files =
      let p = String.length t.path in
      List.fold_left
        (fun acc file ->
          let n = String.length file in
          if n <= p + 1 then acc
          else
            let file = String.with_range file ~first:(p + 1) in
            file :: acc)
        [] files
    in
    List.fold_left
      (fun acc file ->
        match Irmin.Type.of_string K.t (S.key_of_file file) with
        | Ok k -> k :: acc
        | Error (`Msg e) ->
            Log.err (fun l -> l "Irmin_fs.list: %s" e);
            acc)
      [] files

  let clear t =
    Log.debug (fun f -> f "clear");
    let remove_file key =
      IO.remove_file ~lock:(lock_of_key t key) (file_of_key t key)
    in
    list t >>= Lwt_list.iter_p remove_file
end

module Append_only_ext
    (IO : IO)
    (S : Config)
    (K : Irmin.Type.S)
    (V : Irmin.Type.S) =
struct
  include Read_only_ext (IO) (S) (K) (V)

  let temp_dir t = t.path / "tmp"
  let to_bin_string = Irmin.Type.(unstage (to_bin_string V.t))

  let add t key value =
    Log.debug (fun f -> f "add %a" pp_key key);
    let file = file_of_key t key in
    let temp_dir = temp_dir t in
    IO.file_exists file >>= function
    | true -> Lwt.return_unit
    | false ->
        let str = to_bin_string value in
        IO.write_file ~temp_dir file str
end

module Atomic_write_ext
    (IO : IO)
    (S : Config)
    (K : Irmin.Type.S)
    (V : Irmin.Type.S) =
struct
  module RO = Read_only_ext (IO) (S) (K) (V)
  module W = Irmin.Private.Watch.Make (K) (V)

  type t = { t : unit RO.t; w : W.t }
  type key = RO.key
  type value = RO.value
  type watch = W.watch * (unit -> unit Lwt.t)

  let temp_dir t = t.t.RO.path / "tmp"

  module E = Ephemeron.K1.Make (struct
    type t = string

    let equal x y = compare x y = 0
    let hash = Hashtbl.hash
  end)

  let watches = E.create 10

  let v config =
    let+ t = RO.v config in
    let w =
      let path = RO.get_path config in
      try E.find watches path
      with Not_found ->
        let w = W.v () in
        E.add watches path w;
        w
    in
    { t; w }

  let close t = W.clear t.w >>= fun () -> RO.close t.t
  let find t = RO.find t.t
  let mem t = RO.mem t.t
  let list t = RO.list t.t

  let listen_dir t =
    let dir = S.dir t.t.RO.path in
    let key file =
      match Irmin.Type.of_string K.t file with
      | Ok t -> Some t
      | Error (`Msg e) ->
          Log.err (fun l -> l "listen_dir: %s" e);
          None
    in
    W.listen_dir t.w dir ~key ~value:(RO.find t.t)

  let watch_key t key ?init f =
    let* stop = listen_dir t in
    let+ w = W.watch_key t.w key ?init f in
    (w, stop)

  let watch t ?init f =
    let* stop = listen_dir t in
    let+ w = W.watch t.w ?init f in
    (w, stop)

  let unwatch t (id, stop) = stop () >>= fun () -> W.unwatch t.w id
  let raw_value = Irmin.Type.(unstage (to_bin_string V.t))

  let set t key value =
    Log.debug (fun f -> f "update %a" RO.pp_key key);
    let temp_dir = temp_dir t in
    let file = RO.file_of_key t.t key in
    let lock = RO.lock_of_key t.t key in
    IO.write_file ~temp_dir file ~lock (raw_value value) >>= fun () ->
    W.notify t.w key (Some value)

  let remove t key =
    Log.debug (fun f -> f "remove %a" RO.pp_key key);
    let file = RO.file_of_key t.t key in
    let lock = RO.lock_of_key t.t key in
    let* () = IO.remove_file ~lock file in
    W.notify t.w key None

  let test_and_set t key ~test ~set =
    Log.debug (fun f -> f "test_and_set %a" RO.pp_key key);
    let temp_dir = temp_dir t in
    let file = RO.file_of_key t.t key in
    let lock = RO.lock_of_key t.t key in
    let raw_value = function None -> None | Some v -> Some (raw_value v) in
    let* b =
      IO.test_and_set_file file ~temp_dir ~lock ~test:(raw_value test)
        ~set:(raw_value set)
    in
    let+ () = if b then W.notify t.w key set else Lwt.return_unit in
    b

  let clear t =
    Log.debug (fun f -> f "clear");
    let remove_file key =
      IO.remove_file ~lock:(RO.lock_of_key t.t key) (RO.file_of_key t.t key)
    in
    list t >>= Lwt_list.iter_p remove_file
end

module Maker_ext (IO : IO) (Obj : Config) (Ref : Config) = struct
  module AO = Append_only_ext (IO) (Obj)
  module AW = Atomic_write_ext (IO) (Ref)
  module CA = Irmin.Content_addressable.Make (AO)
  include Irmin.Maker (CA) (AW)
end

let string_chop_prefix ~prefix str =
  let len = String.length prefix in
  if String.length str <= len then "" else String.with_range str ~first:len

module Ref = struct
  let dir p = p / "refs"

  (* separator for branch names is '/', so need to rewrite the path on
     Windows. *)

  let file_of_key key =
    let file =
      if Sys.os_type <> "Win32" then key
      else String.concat ~sep:Filename.dir_sep (String.cuts ~sep:"/" key)
    in
    "refs" / file

  let key_of_file file =
    let key = string_chop_prefix ~prefix:("refs" / "") file in
    if Sys.os_type <> "Win32" then key
    else String.concat ~sep:"/" (String.cuts ~sep:Filename.dir_sep key)
end

module Obj = struct
  let dir t = t / "objects"

  let file_of_key k =
    let pre = String.with_range k ~len:2 in
    let suf = String.with_range k ~first:2 in
    "objects" / pre / suf

  let key_of_file path =
    let path = string_chop_prefix ~prefix:("objects" / "") path in
    let path = String.cuts ~sep:Filename.dir_sep path in
    let path = String.concat ~sep:"" path in
    path
end

module Append_only (IO : IO) = Append_only_ext (IO) (Obj)
module Atomic_write (IO : IO) = Atomic_write_ext (IO) (Ref)
module Maker (IO : IO) = Maker_ext (IO) (Obj) (Ref)

module KV (IO : IO) = struct
  module AO = Append_only (IO)
  module AW = Atomic_write (IO)
  module CA = Irmin.Content_addressable.Make (AO)
  include Irmin.KV_maker (CA) (AW)
end

module IO_mem = struct
  type t = {
    watches : (string, string -> unit Lwt.t) Hashtbl.t;
    files : (string, string) Hashtbl.t;
  }

  let t = { watches = Hashtbl.create 3; files = Hashtbl.create 13 }

  type path = string
  type lock = Lwt_mutex.t

  let locks = Hashtbl.create 10

  let lock_file file =
    try Hashtbl.find locks file
    with Not_found ->
      let l = Lwt_mutex.create () in
      Hashtbl.add locks file l;
      l

  let with_lock l f =
    match l with None -> f () | Some l -> Lwt_mutex.with_lock l f

  let set_listen_hook () =
    let h _ dir f =
      Hashtbl.replace t.watches dir f;
      Lwt.return (fun () ->
          Hashtbl.remove t.watches dir;
          Lwt.return_unit)
    in
    Irmin.Private.Watch.set_listen_dir_hook h

  let notify file =
    Hashtbl.fold
      (fun dir f acc ->
        if String.is_prefix ~affix:dir file then f file :: acc else acc)
      t.watches []
    |> Lwt_list.iter_p (fun x -> x)

  let mkdir _ = Lwt.return_unit

  let remove_file ?lock file =
    with_lock lock (fun () ->
        Hashtbl.remove t.files file;
        Lwt.return_unit)

  let rec_files dir =
    Hashtbl.fold
      (fun k _ acc -> if String.is_prefix ~affix:dir k then k :: acc else acc)
      t.files []
    |> Lwt.return

  let file_exists file = Hashtbl.mem t.files file |> Lwt.return

  let read_file file =
    try
      let buf = Hashtbl.find t.files file in
      Lwt.return_some buf
    with Not_found -> Lwt.return_none

  let write_file ?temp_dir:_ ?lock file v =
    let* () =
      with_lock lock (fun () ->
          Hashtbl.replace t.files file v;
          Lwt.return_unit)
    in
    notify file

  let equal x y =
    match (x, y) with
    | None, None -> true
    | Some x, Some y -> String.equal x y
    | _ -> false

  let test_and_set_file ?temp_dir:_ ~lock file ~test ~set =
    let f () =
      let old = try Some (Hashtbl.find t.files file) with Not_found -> None in
      let b =
        if not (equal old test) then false
        else
          match set with
          | None ->
              Hashtbl.remove t.files file;
              true
          | Some v ->
              Hashtbl.replace t.files file v;
              true
      in
      let+ () = if b then notify file else Lwt.return_unit in
      b
    in
    with_lock (Some lock) f

  let clear () =
    Hashtbl.clear t.files;
    Hashtbl.clear t.watches;
    Lwt.return_unit
end

(* Enforce that {!S} is a sub-type of {!Irmin.Maker}. *)
module Maker_is_a_maker : Irmin.Maker = Maker (IO_mem)

(* Enforce that {!KV} is a sub-type of {!Irmin.KV_maker}. *)
module KV_is_a_KV : Irmin.KV_maker = KV (IO_mem)
