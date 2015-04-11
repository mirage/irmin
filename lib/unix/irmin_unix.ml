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
      Lwt_unix.stat file >>= fun s ->
      let stale = Unix.gettimeofday () -. s.Unix.st_mtime > max_age in
      Lwt.return stale
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
  module Memory = Irmin_git.Memory(Git_unix.Sync.IO)
  module FS = Irmin_git.FS(Git_unix.Sync.IO)(Lock)(IO)
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
      Printf.sprintf "%2d:%2d:%2d" tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec
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

let stop = ref (fun () -> ())

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

let with_cancel t =
  Lwt.catch t (function Lwt.Canceled -> Lwt.return_unit | e -> Lwt.fail e)

let install_dir_polling_listener delay =
  let s, u = Lwt.task () in
  !stop ();
  stop := Lwt.wakeup u;

  (* map directory names to list of callbacks *)
  let listeners = Hashtbl.create 10 in

  let listen_dir id dir fn =

    (* add the listener *)
    let add_listener () =
      let fns = try Hashtbl.find listeners dir with Not_found -> [] in
      let fns = (id, fn) :: fns in
      Hashtbl.replace listeners dir fns
    in

    (* remove the listener *)
    let remove_listener () =
      let fns = try Hashtbl.find listeners dir with Not_found -> [] in
      let fns = List.filter (fun (x,_) -> x <> id) fns in
      if fns = [] then Hashtbl.remove listeners dir
      else Hashtbl.replace listeners dir fns
    in

    (* call the callbacks on the file *)
    let callback file =
      let fns = try Hashtbl.find listeners dir with Not_found -> [] in
      Lwt_list.iter_p (fun (id, f) -> Log.debug "callback %d" id; f file) fns
    in

    (* active polling *)
    let rec loop files =
      read_files dir >>= fun new_files ->
      let diff = S.sdiff files new_files in
      if not (S.is_empty diff) then
        Log.debug "polling %d %s: diff:%s" id dir (to_string diff)
      else
        Log.debug "polling %d no changes!" id;
      Lwt_list.iter_p (fun (f, _) -> callback f) (S.to_list diff) >>= fun () ->
      Lwt_unix.sleep delay >>= fun () ->
      loop new_files
    in

    let listen () = with_cancel (fun () -> read_files dir >>= loop) in

    let tr = ref None in

    let start_watchdog () =
      if not (Hashtbl.mem listeners dir) then (
        Log.debug "Start watchdog for %s" dir;
        let t = listen () in
        tr := Some t;
        Lwt.async (fun () -> Lwt.pick [s; t])
      )
    in
    let stop_watchdog () =
      match Hashtbl.length listeners, !tr with
      | 0, Some t ->
        Log.debug "Stop watchdog for %s" dir;
        tr := None;
        Lwt.cancel t
      | _ -> ()
    in
    (* run the background thread if it is not already running. *)
    start_watchdog ();
    add_listener ();
    (fun () -> remove_listener (); stop_watchdog ())

  in
  Irmin.Private.Watch.set_listen_dir_hook listen_dir

let uninstall_dir_polling_listener () =
  !stop ();
  stop := (fun () -> ())

let task msg =
  let date = Int64.of_float (Unix.gettimeofday ()) in
  let owner =
    (* XXX: get "git config user.name" *)
    Printf.sprintf "Irmin %s.[%d]" (Unix.gethostname()) (Unix.getpid())
  in
  Irmin.Task.create ~date ~owner msg
