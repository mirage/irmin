(*
 * Copyright (c) 2013-2022 Thomas Gazagnaire <thomas@gazagnaire.org>
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
open Eio
open Astring

let src = Logs.Src.create "irmin.fs" ~doc:"Irmin disk persistence"

module Log = (val Logs.src_log src : Logs.LOG)

let ( / ) = Path.( / )

module type Config = sig
  val dir : Fs.dir_ty Path.t -> Fs.dir_ty Path.t
  val file_of_key : string -> string
  val key_of_file : string -> string
end

module type IO = sig
  type path = Fs.dir_ty Path.t

  val rec_files : path -> path list
  val file_exists : path -> bool
  val read_file : path -> string option
  val mkdir : path -> unit

  type lock

  val lock_file : path -> lock
  val write_file : ?temp_dir:path -> ?lock:lock -> path -> string -> unit

  val test_and_set_file :
    ?temp_dir:path ->
    lock:lock ->
    path ->
    test:string option ->
    set:string option ->
    bool

  val remove_file : ?lock:lock -> path -> unit
end

(* ~path *)

module Conf = struct
  include Irmin.Backend.Conf

  let spec = Spec.v "ifs"

  module Key = struct
    let root = root spec
  end
end

let config r = Conf.(verify (add (empty Conf.spec) Key.root r))

module Read_only_ext
    (IO : IO)
    (S : Config)
    (K : Irmin.Type.S)
    (V : Irmin.Type.S) =
struct
  type key = K.t
  type value = V.t
  type 'a t = { path : Fs.dir_ty Path.t }

  let get_path config = Option.value Conf.(find_root config) ~default:"."

  let v ~sw:_ config =
    let fs = Irmin.Backend.Conf.Env.fs () in
    let path = Path.(fs / get_path config) in
    IO.mkdir path;
    { path }

  let close _ = ()
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
        [%log.err "Irmin_fs.value %s" e];
        None

  let pp_key = Irmin.Type.pp K.t

  let find t key =
    [%log.debug "find %a" pp_key key];
    match IO.read_file (file_of_key t key) with
    | None -> None
    | Some x -> value x

  let list t =
    [%log.debug "list"];
    let files = IO.rec_files (S.dir t.path) in
    let files =
      let p = String.length (snd t.path) in
      List.fold_left
        (fun acc (_, file) ->
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
            [%log.err "Irmin_fs.list: %s" e];
            acc)
      [] files
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
    [%log.debug "add %a" pp_key key];
    let file = file_of_key t key in
    let temp_dir = temp_dir t in
    match IO.file_exists file with
    | true -> ()
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
  module W = Irmin.Backend.Watch.Make (K) (V)

  type t = { t : unit RO.t; w : W.t }
  type key = RO.key
  type value = RO.value
  type watch = W.watch * (unit -> unit)

  let temp_dir t = t.t.RO.path / "tmp"

  module E = Ephemeron.K1.Make (struct
    type t = string

    let equal x y = compare x y = 0
    let hash = Hashtbl.hash
  end)

  let watches = E.create 10

  let v ~sw config =
    let t = RO.v ~sw config in
    let w =
      let path = RO.get_path config in
      try E.find watches path
      with Not_found ->
        let w = W.v () in
        E.add watches path w;
        w
    in
    { t; w }

  let close t =
    W.clear t.w;
    RO.close t.t

  let find t = RO.find t.t
  let mem t = RO.mem t.t
  let list t = RO.list t.t

  let listen_dir t =
    let dir = S.dir t.t.RO.path in
    let key file =
      match Irmin.Type.of_string K.t file with
      | Ok t -> Some t
      | Error (`Msg e) ->
          [%log.err "listen_dir: %s" e];
          None
    in
    W.listen_dir t.w (snd dir) ~key ~value:(RO.find t.t)

  let watch_key t key ?init f =
    let stop = listen_dir t in
    let w = W.watch_key t.w key ?init f in
    (w, stop)

  let watch t ?init f =
    let stop = listen_dir t in
    let w = W.watch t.w ?init f in
    (w, stop)

  let unwatch t (id, stop) =
    stop ();
    W.unwatch t.w id

  let raw_value = Irmin.Type.(unstage (to_bin_string V.t))

  let set t key value =
    [%log.debug "update %a" RO.pp_key key];
    let temp_dir = temp_dir t in
    let file = RO.file_of_key t.t key in
    let lock = RO.lock_of_key t.t key in
    IO.write_file ~temp_dir file ~lock (raw_value value);
    W.notify t.w key (Some value)

  let remove t key =
    [%log.debug "remove %a" RO.pp_key key];
    let file = RO.file_of_key t.t key in
    let lock = RO.lock_of_key t.t key in
    let () = IO.remove_file ~lock file in
    W.notify t.w key None

  let test_and_set t key ~test ~set =
    [%log.debug "test_and_set %a" RO.pp_key key];
    let temp_dir = temp_dir t in
    let file = RO.file_of_key t.t key in
    let lock = RO.lock_of_key t.t key in
    let raw_value = function None -> None | Some v -> Some (raw_value v) in
    let b =
      IO.test_and_set_file file ~temp_dir ~lock ~test:(raw_value test)
        ~set:(raw_value set)
    in
    let () = if b then W.notify t.w key set in
    b

  let clear t =
    [%log.debug "clear"];
    let remove_file key () =
      IO.remove_file ~lock:(RO.lock_of_key t.t key) (RO.file_of_key t.t key)
    in
    list t |> fun keys -> Eio.Fiber.all (List.map remove_file keys)
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
    Filename.concat "refs" file

  let key_of_file file =
    let key = string_chop_prefix ~prefix:(Filename.concat "refs" "") file in
    if Sys.os_type <> "Win32" then key
    else String.concat ~sep:"/" (String.cuts ~sep:Filename.dir_sep key)
end

module Obj = struct
  let dir t = t / "objects"

  let file_of_key k =
    let pre = String.with_range k ~len:2 in
    let suf = String.with_range k ~first:2 in
    let ( / ) = Filename.concat in
    "objects" / pre / suf

  let key_of_file path =
    let ( / ) = Filename.concat in
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
    watches : (string, string -> unit) Hashtbl.t;
    files : (Fs.dir_ty Path.t, string) Hashtbl.t;
  }

  let t = { watches = Hashtbl.create 3; files = Hashtbl.create 13 }

  type path = Fs.dir_ty Path.t
  type lock = Eio.Mutex.t

  let locks = Hashtbl.create 10

  let lock_file (_, file) =
    try Hashtbl.find locks file
    with Not_found ->
      let l = Eio.Mutex.create () in
      Hashtbl.add locks file l;
      l

  let with_lock l f =
    match l with None -> f () | Some l -> Eio.Mutex.use_rw ~protect:false l f

  let set_listen_hook () =
    let h _ dir f =
      Hashtbl.replace t.watches dir f;
      fun () -> Hashtbl.remove t.watches dir
    in
    Irmin.Backend.Watch.set_listen_dir_hook h

  let notify file =
    Hashtbl.iter
      (fun dir f -> if String.is_prefix ~affix:dir file then f file)
      t.watches
  (* |> Eio.Fiber.all *)

  let mkdir _ = ()

  let remove_file ?lock file =
    with_lock lock (fun () -> Hashtbl.remove t.files file)

  let rec_files (_, dir) =
    Hashtbl.fold
      (fun ((_, k) as v) _ acc ->
        if String.is_prefix ~affix:dir k then v :: acc else acc)
      t.files []

  let file_exists file = Hashtbl.mem t.files file

  let read_file file =
    try
      let buf = Hashtbl.find t.files file in
      Some buf
    with Not_found -> None

  let write_file ?temp_dir:_ ?lock ((_, file) as f) v =
    let () = with_lock lock (fun () -> Hashtbl.replace t.files f v) in
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
      let () = if b then notify (snd file) in
      b
    in
    with_lock (Some lock) f

  let clear () =
    Hashtbl.clear t.files;
    Hashtbl.clear t.watches
end

(* Enforce that {!S} is a sub-type of {!Irmin.Maker}. *)
module Maker_is_a_maker : Irmin.Maker = Maker (IO_mem)

(* Enforce that {!KV} is a sub-type of {!Irmin.KV_maker}. *)
module KV_is_a_KV : Irmin.KV_maker = KV (IO_mem)

let run (fs : Fs.dir_ty Path.t) fn =
  Switch.run @@ fun sw ->
  Irmin.Backend.Watch.set_watch_switch sw;
  let open Effect.Deep in
  try_with fn ()
    {
      effc =
        (fun (type a) (e : a Effect.t) ->
          match e with
          | Irmin.Backend.Conf.Env.Fs ->
              Some (fun (k : (a, _) continuation) -> continue k fs)
          | _ -> None);
    }
