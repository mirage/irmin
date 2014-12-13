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
module IB = Irmin.Private

module Log = Log.Make(struct let section = "UNIX" end)

module S = struct
  module X = struct
    include Set.Make(struct
        type t = string * string
        let compare = Tc.Compare.pair String.compare String.compare
      end)
    let of_list l = List.fold_left (fun set elt -> add elt set) empty l
    let to_list = elements
    module K = Tc.Pair(Tc.String)(Tc.String)
  end
  include X
  include Tc.As_L0 (X)
end

let (/) = Filename.concat

module IO = Git_unix.FS.IO

module Irmin_fs = struct
  let config = Irmin_fs.config
  module AO = Irmin_fs.AO(IO)
  module RW = Irmin_fs.RW(IO)
  module Make = Irmin_fs.Make(IO)
  module type Config = Irmin_fs.Config
  module AO_ext = Irmin_fs.AO_ext(IO)
  module RW_ext = Irmin_fs.RW_ext(IO)
  module Make_ext = Irmin_fs.Make_ext(IO)
end

module Irmin_git = struct
  let config = Irmin_git.config
  let branch = Irmin_git.branch
  let bare = Irmin_git.bare
  module AO = Irmin_git.AO
  module RW = Irmin_git.RW
  module Memory = Irmin_git.Memory(Git_unix.Sync.IO)
  module FS = Irmin_git.FS(Git_unix.Sync.IO)(IO)
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
  module type S = Irmin_http_server.S
  module Make = Irmin_http_server.Make (X)(Y)
end


let install_dir_polling_listener delay =

  Irmin.Watch.set_listen_dir_hook (fun dir fn ->

      let read_files () =
        IO.rec_files dir >>= fun new_files ->
        let new_files = List.map (fun f -> let f = dir / f in f, Digest.file f) new_files in
        return (S.of_list new_files)
      in

      let to_string set = Tc.show (module S) set in

      let rec loop files =
        read_files () >>= fun new_files ->
        let diff = S.diff files new_files in
        if not (S.is_empty diff) then
          Log.debugf "polling %s: diff:%s" dir (to_string diff);
        Lwt_list.iter_p (fun (f, _) -> fn f) (S.to_list diff) >>= fun () ->
        Lwt_unix.sleep delay >>= fun () ->
        loop new_files
      in

      let t () =
        read_files () >>= fun new_files ->
        loop new_files
      in

      Lwt.async t
    )

let task msg =
  let date = Int64.of_float (Unix.gettimeofday ()) in
  let owner =
    (* XXX: get "git config user.name" *)
    Printf.sprintf "Irmin %s.[%d]" (Unix.gethostname()) (Unix.getpid())
  in
  Irmin.Task.create ~date ~owner msg
