(*
 * Copyright (c) 2013-2015 Thomas Gazagnaire <thomas@gazagnaire.org>
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
module IB = Irmin.Private

module Log = Log.Make(struct let section = "UNIX" end)

module IO = Git_unix.FS.IO

module type LOCK = Irmin_fs.LOCK

module Lock = struct

  let is_stale max_age file =
    IO.file_exists file >>= fun exists ->
    if exists then (
      Lwt.catch (fun () ->
          Lwt_unix.stat file >>= fun s ->
          let stale = Unix.gettimeofday () -. s.Unix.st_mtime > max_age in
          Lwt.return stale)
        (function
          | Unix.Unix_error (Unix.ENOENT, _, _) -> Lwt.return false
          | e -> Lwt.fail e)
    ) else
      Lwt.return false

  let unlock file =
    IO.remove file

  let lock ?(max_age = 2.) ?(sleep = 0.001) file =
    let rec aux i =
      Log.debug "lock %d" i;
      is_stale max_age file >>= fun is_stale ->
      if is_stale then (
        Log.error "%s is stale, removing it." file;
        unlock file >>= fun () ->
        aux 1
      ) else
        let create () =
          let pid = Unix.getpid () in
          IO.mkdir (Filename.dirname file) >>= fun () ->
          Lwt_unix.openfile file [Unix.O_CREAT; Unix.O_RDWR; Unix.O_EXCL] 0o600
          >>= fun fd ->
          let oc = Lwt_io.of_fd ~mode:Lwt_io.Output fd in
          Lwt_io.write_int oc pid >>= fun () ->
          Lwt_unix.close fd
        in
        Lwt.catch create (function
            | Unix.Unix_error(Unix.EEXIST, _, _) ->
              let backoff = 1. +. Random.float (let i = float i in i *. i) in
              Lwt_unix.sleep (sleep *. backoff) >>= fun () ->
              aux (i+1)
            | e -> Lwt.fail e)
    in
    aux 1

  let with_lock file fn =
    lock file >>= fun () ->
    Lwt.finalize fn (fun () -> unlock file)

end

module Irmin_fs = struct
  let config = Irmin_fs.config
  module AO = Irmin_fs.AO(IO)
  module RW = Irmin_fs.RW(IO)(Lock)
  module Make = Irmin_fs.Make(IO)(Lock)
  module type Config = Irmin_fs.Config
  module AO_ext = Irmin_fs.AO_ext(IO)
  module RW_ext = Irmin_fs.RW_ext(IO)(Lock)
  module Make_ext = Irmin_fs.Make_ext(IO)(Lock)
end

module Irmin_git = struct
  let config = Irmin_git.config
  let head = Irmin_git.head
  let bare = Irmin_git.bare
  module AO = Irmin_git.AO
  module RW = Irmin_git.RW(Lock)
  module Memory = Irmin_git.Memory(Git_unix.Sync.IO)(Git_unix.Zlib)
  module FS = Irmin_git.FS(Git_unix.Sync.IO)(Git_unix.Zlib)(Lock)(IO)
end

module Irmin_http = struct
  let config = Irmin_http.config
  let uri = Irmin_http.uri
  module AO = Irmin_http.AO(Cohttp_lwt_unix.Client)
  module RW = Irmin_http.RW(Cohttp_lwt_unix.Client)
  module Make = Irmin_http.Make(Cohttp_lwt_unix.Client)
  module Low = Irmin_http.Low(Cohttp_lwt_unix.Client)
end

module Irmin_http_server = struct
  module X = struct
    include Cohttp_lwt_unix.Server
    let listen t ?timeout uri =
      let port = match Uri.port uri with
        | None   -> 8080
        | Some p -> p in
      create ?timeout ~mode:(`TCP (`Port port)) t
  end
  module Y = struct
    let pretty d =
      let tm = Unix.localtime (Int64.to_float d) in
      Printf.sprintf "%02d:%02d:%02d"
        tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec
  end
  type hooks = Irmin_http_server.hooks = { update: unit -> unit Lwt.t }
  module type S = Irmin_http_server.S
  module Make = Irmin_http_server.Make (X)(Y)
end

module S = struct
  module X = struct
    include Set.Make(struct
        type t = string * string
        let compare = Tc.Compare.pair String.compare String.compare
      end)
    let of_list l = List.fold_left (fun set elt -> add elt set) empty l
    let to_list = elements
    module K = Tc.Pair(Tc.String)(Tc.String)
    let sdiff x y = union (diff x y) (diff y x)
  end
  include X
  include Tc.As_L0 (X)
end

module StringSet = struct
  include Set.Make(Tc.String)
  let of_list l = List.fold_left (fun acc e -> add e acc) empty l
end

let to_string set = Tc.show (module S) set

let string_chop_prefix t ~prefix =
  let lt = String.length t in
  let lp = String.length prefix in
  if lt < lp then None else
    let p = String.sub t 0 lp in
    if String.compare p prefix <> 0 then None
    else Some (String.sub t lp (lt - lp))

let string_chop_prefix_exn t ~prefix = match string_chop_prefix t ~prefix with
  | None   -> failwith "string_chop_prefix"
  | Some s -> s

let (/) = Filename.concat

let read_files dir =
  IO.rec_files dir >>= fun new_files ->
  let prefix = dir / "" in
  let new_files =
    List.map (fun f -> string_chop_prefix_exn f ~prefix, Digest.file f) new_files
  in
  return (S.of_list new_files)

(* run [t] and returns an handler to stop the task. *)
let stoppable t =
  let s, u = Lwt.task () in
  Lwt.async (fun () -> Lwt.pick ([s; t ()]));
  function () -> Lwt.wakeup u ()

(* active polling *)
let rec poll ~callback ~delay dir files =
  read_files dir >>= fun new_files ->
  let diff = S.sdiff files new_files in
  begin if S.is_empty diff then (
      Log.debug "polling %s: no changes!" dir;
      Lwt.return_unit
    ) else (
      Log.debug "polling %s: diff:%s" dir (to_string diff);
      let files =
        S.to_list diff |> List.map fst |> StringSet.of_list |> StringSet.elements
      in
      Lwt_list.iter_p (callback dir) files)
  end >>= fun () ->
  Lwt_unix.sleep delay >>= fun () ->
  poll ~callback ~delay dir new_files

let listen ~callback ~delay dir =
  read_files dir >|= fun files ->
  stoppable (fun () -> poll ~callback ~delay dir files)

(* map directory names to list of callbacks *)
let listeners = Hashtbl.create 10
let watchdogs = Hashtbl.create 10

let nb_listeners dir =
  try List.length (Hashtbl.find listeners dir) with Not_found -> 0

let watchdog dir =
  try Some (Hashtbl.find watchdogs dir) with Not_found -> None

(* call all the callbacks on the file *)
let callback dir file =
  let fns = try Hashtbl.find listeners dir with Not_found -> [] in
  Lwt_list.iter_p (fun (id, f) -> Log.debug "callback %d" id; f file) fns

let realdir dir = if Filename.is_relative dir then Sys.getcwd () / dir else dir

let start_watchdog ~delay dir =
  match watchdog dir with
  | Some _ -> assert (nb_listeners dir <> 0); Lwt.return_unit
  | None   ->
    Log.debug "Start watchdog for %s" dir;
    listen dir ~delay ~callback >|= fun u ->
    Hashtbl.add watchdogs dir u

let stop_watchdog dir =
  match watchdog dir with
  | None      -> assert (nb_listeners dir = 0)
  | Some stop ->
    if nb_listeners dir = 0 then (
      Log.debug "Stop watchdog for %s" dir;
      Hashtbl.remove watchdogs dir;
      stop ()
    )

let add_listener id dir fn =
  let fns = try Hashtbl.find listeners dir with Not_found -> [] in
  let fns = (id, fn) :: fns in
  Hashtbl.replace listeners dir fns

let remove_listener id dir =
  let fns = try Hashtbl.find listeners dir with Not_found -> [] in
  let fns = List.filter (fun (x,_) -> x <> id) fns in
  if fns = [] then Hashtbl.remove listeners dir
  else Hashtbl.replace listeners dir fns

let uninstall_dir_polling_listener () =
  Hashtbl.iter (fun _dir stop -> stop ()) watchdogs;
  Hashtbl.clear watchdogs;
  Hashtbl.clear listeners

let install_dir_polling_listener delay =
  uninstall_dir_polling_listener ();
  let listen_dir id dir fn =
    let dir = realdir dir in
    start_watchdog ~delay dir >|= fun () ->
    add_listener id dir fn;
    function () ->
      remove_listener id dir;
      stop_watchdog dir
  in
  Irmin.Private.Watch.set_listen_dir_hook listen_dir

let polling_threads () = Hashtbl.length watchdogs

let task msg =
  let date = Int64.of_float (Unix.gettimeofday ()) in
  let owner =
    (* XXX: get "git config user.name" *)
    Printf.sprintf "Irmin %s.[%d]" (Unix.gethostname()) (Unix.getpid())
  in
  Irmin.Task.create ~date ~owner msg
