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
open Astring

let src = Logs.Src.create "irmin.fs" ~doc:"Irmin disk persistence"

module Log = (val Logs.src_log src : Logs.LOG)

let ( / ) = Filename.concat

module type Config = sig
  val dir : string -> string
  val file_of_key : string -> string
  val key_of_file : string -> string
end

let string_chop_prefix ~prefix str =
  let len = String.length prefix in
  if String.length str <= len then str
  else
    let s = String.with_range ~len str in
    if String.equal s prefix then String.with_range str ~first:len else str

module type IO = sig
  type io

  val io_of_config : Irmin.config -> io

  type path = string

  val rec_files : io:io -> path -> path list
  val file_exists : io:io -> path -> bool
  val read_file : io:io -> path -> string option
  val mkdir : io:io -> path -> unit

  type lock

  val lock_file : io:io -> path -> lock

  val write_file :
    io:io -> temp_dir:path -> ?lock:lock -> path -> string -> unit

  val test_and_set_file :
    io:io ->
    temp_dir:path ->
    lock:lock ->
    path ->
    test:string option ->
    set:string option ->
    bool

  val remove_file : io:io -> ?lock:lock -> path -> unit
  val watch_path : io:io -> path -> string
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
  type 'a t = { path : string; io : IO.io }

  let get_path config = Option.value Conf.(find_root config) ~default:"."

  let v config =
    let io = IO.io_of_config config in
    let path = get_path config in
    IO.mkdir ~io path;
    { path; io }

  let close _ = ()
  let cast t = (t :> read_write t)
  let batch t f = f (cast t)

  let file_of_key { path; _ } key =
    path / S.file_of_key (Irmin.Type.to_string K.t key)

  let lock_of_key { io; path } key =
    IO.lock_file ~io
      (path / "lock" / S.file_of_key (Irmin.Type.to_string K.t key))

  let mem t key =
    let file = file_of_key t key in
    IO.file_exists ~io:t.io file

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
    match IO.read_file ~io:t.io (file_of_key t key) with
    | None -> None
    | Some x -> value x

  let list { path; io } =
    [%log.debug "list"];
    let dir = S.dir path in
    let files = IO.rec_files ~io dir in
    let files =
      List.fold_left
        (fun acc file ->
          let file = string_chop_prefix ~prefix:(Filename.concat dir "") file in
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
    match IO.file_exists ~io:t.io file with
    | true -> ()
    | false ->
        let str = to_bin_string value in
        IO.write_file ~io:t.io ~temp_dir file str
end

module Atomic_write_ext
    (IO : IO)
    (S : Config)
    (K : Irmin.Type.S)
    (V : Irmin.Type.S) =
struct
  module RO = Read_only_ext (IO) (S) (K) (V)
  module W = Irmin.Backend.Watch.Make (K) (V)

  type key = RO.key
  type value = RO.value
  type watch = W.watch * (unit -> unit)

  (* [_watch_key] keeps alive the key of [w] in [watches]: an ephemeron binding
     is kept only while its key is. [own] holds the watches added through this
     store, as [w] is shared with the other stores with the same files, and is
     [None] once the store is closed. *)
  type t = {
    t : unit RO.t;
    w : W.t;
    _watch_key : string;
    own : watch list option Atomic.t;
  }

  let temp_dir t = t.t.RO.path / "tmp"

  module E = Ephemeron.K1.Make (struct
    type t = string

    let equal x y = compare x y = 0
    let hash = Hashtbl.hash
  end)

  let watches = E.create 10

  let v config =
    let t = RO.v config in
    (* Stores with the same files share their watch state. *)
    let _watch_key, w =
      let path = IO.watch_path ~io:t.RO.io t.RO.path in
      try E.find watches path
      with Not_found ->
        let entry = (path, W.v ()) in
        E.add watches path entry;
        entry
    in
    { t; w; _watch_key; own = Atomic.make (Some []) }

  (* [update_own t f] applies [f] to the watches added through [t], and is
     [false] if [t] is closed. *)
  let rec update_own t f =
    match Atomic.get t.own with
    | None -> false
    | Some own as old ->
        Atomic.compare_and_set t.own old (Some (f own)) || update_own t f

  (* Removing a watch cleans up, so it is not cancelled. The callback is
     removed first, in case the hook fails to stop. *)
  let remove_watch t (id, stop) =
    Eio.Cancel.protect @@ fun () ->
    W.unwatch t.w id;
    stop ()

  (* Closing a store removes its watches only. Only the last removal stops the
     hook, so an error there leaves no watch behind. *)
  let close t =
    Atomic.exchange t.own None |> Option.iter (List.iter (remove_watch t));
    RO.close t.t

  let find t = RO.find t.t
  let mem t = RO.mem t.t
  let list t = RO.list t.t

  let listen_dir t =
    let dir = IO.watch_path ~io:t.t.RO.io (S.dir t.t.RO.path) in
    let key file =
      match Irmin.Type.of_string K.t file with
      | Ok t -> Some t
      | Error (`Msg e) ->
          [%log.err "listen_dir: %s" e];
          None
    in
    (* The hook of the shared state reads the files through the store that
       started it, also once it is closed: [RO.close] releases nothing. *)
    W.listen_dir t.w dir ~key ~value:(RO.find t.t)

  (* [add_watch t register] starts the hook and adds the watch [register ()].
     [t] may be closed while the hook starts. *)
  let add_watch t register =
    let stop = listen_dir t in
    let id =
      try register ()
      with e ->
        let bt = Printexc.get_raw_backtrace () in
        stop ();
        Printexc.raise_with_backtrace e bt
    in
    let watch = (id, stop) in
    if update_own t (List.cons watch) then watch
    else (
      remove_watch t watch;
      raise Irmin.Closed)

  let watch_key t key ?init f =
    add_watch t (fun () -> W.watch_key t.w key ?init f)

  let watch t ?init f = add_watch t (fun () -> W.watch t.w ?init f)

  (* The stop function of a watch identifies it: [W.listen_dir] returns a new
     one at each call. *)
  let unwatch t ((_, stop) as watch) =
    let _ : bool =
      update_own t (List.filter (fun (_, stop') -> stop' != stop))
    in
    remove_watch t watch

  let raw_value = Irmin.Type.(unstage (to_bin_string V.t))

  let set t key value =
    [%log.debug "update %a" RO.pp_key key];
    let temp_dir = temp_dir t in
    let file = RO.file_of_key t.t key in
    let lock = RO.lock_of_key t.t key in
    IO.write_file ~io:t.t.io ~temp_dir file ~lock (raw_value value);
    W.notify t.w key (Some value)

  let remove t key =
    [%log.debug "remove %a" RO.pp_key key];
    let file = RO.file_of_key t.t key in
    let lock = RO.lock_of_key t.t key in
    let () = IO.remove_file ~io:t.t.io ~lock file in
    W.notify t.w key None

  let test_and_set t key ~test ~set =
    [%log.debug "test_and_set %a" RO.pp_key key];
    let temp_dir = temp_dir t in
    let file = RO.file_of_key t.t key in
    let lock = RO.lock_of_key t.t key in
    let raw_value = function None -> None | Some v -> Some (raw_value v) in
    let b =
      IO.test_and_set_file ~io:t.t.io file ~temp_dir ~lock
        ~test:(raw_value test) ~set:(raw_value set)
    in
    let () = if b then W.notify t.w key set in
    b

  let clear t =
    [%log.debug "clear"];
    let remove_file key () =
      IO.remove_file ~io:t.t.io ~lock:(RO.lock_of_key t.t key)
        (RO.file_of_key t.t key)
    in
    list t |> fun keys -> Eio.Fiber.all (List.map remove_file keys)
end

module Maker_ext (IO : IO) (Obj : Config) (Ref : Config) = struct
  module AO = Append_only_ext (IO) (Obj)
  module AW = Atomic_write_ext (IO) (Ref)
  module CA = Irmin.Content_addressable.Make (AO)
  include Irmin.Maker (CA) (AW)
end

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
  type io = unit

  let io_of_config _ = ()

  type path = string

  type t = {
    watches : (string, string -> unit) Hashtbl.t;
    files : (path, string) Hashtbl.t;
  }

  let t = { watches = Hashtbl.create 3; files = Hashtbl.create 13 }

  type lock = Eio.Mutex.t

  let locks = Hashtbl.create 10

  let lock_file ~io:() file =
    try Hashtbl.find locks file
    with Not_found ->
      let l = Eio.Mutex.create () in
      Hashtbl.add locks file l;
      l

  let with_lock ~io:() l f =
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

  let mkdir ~io:() _ = ()

  let remove_file ~io ?lock file =
    with_lock ~io lock (fun () -> Hashtbl.remove t.files file)

  let watch_path ~io:() dir = dir

  let rec_files ~io:() dir =
    Hashtbl.fold
      (fun file _ acc ->
        if String.is_prefix ~affix:dir file then file :: acc else acc)
      t.files []

  let file_exists ~io:() file = Hashtbl.mem t.files file

  let read_file ~io:() file =
    try
      let buf = Hashtbl.find t.files file in
      Some buf
    with Not_found -> None

  let write_file ~io ~temp_dir:_ ?(lock : lock option) file v =
    let () = with_lock ~io lock (fun () -> Hashtbl.replace t.files file v) in
    notify file

  let equal x y =
    match (x, y) with
    | None, None -> true
    | Some x, Some y -> String.equal x y
    | _ -> false

  let test_and_set_file ~io ~temp_dir:_ ~lock file ~test ~set =
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
      let () = if b then notify file in
      b
    in
    with_lock ~io (Some lock) f

  let clear () =
    Hashtbl.clear t.files;
    Hashtbl.clear t.watches
end

(* Enforce that {!S} is a sub-type of {!Irmin.Maker}. *)
module Maker_is_a_maker : Irmin.Maker = Maker (IO_mem)

(* Enforce that {!KV} is a sub-type of {!Irmin.KV_maker}. *)
module KV_is_a_KV : Irmin.KV_maker = KV (IO_mem)
