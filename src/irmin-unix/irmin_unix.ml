(*
 * Copyright (c) 2013-2017 Thomas Gazagnaire <thomas@gazagnaire.org>
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

open Lwt.Infix

module IO = struct
  include Git_unix.FS.IO
  let rec_files dir =
    let rec aux accu dir =
      directories dir >>= fun ds ->
      files dir       >>= fun fs ->
      Lwt_list.fold_left_s aux (fs @ accu) ds in
    aux [] dir
end

module FS = struct
  module AO = Irmin_fs.AO(IO)
  module Link = Irmin_fs.Link(IO)
  module RW = Irmin_fs.RW(IO)
  module Make = Irmin_fs.Make(IO)
  module KV = Irmin_fs.KV(IO)
  module AO_ext = Irmin_fs.AO_ext(IO)
  module RW_ext = Irmin_fs.RW_ext(IO)
  module Make_ext = Irmin_fs.Make_ext(IO)
end

module Git = struct
  module AO = Irmin_git.AO
  module RW = Irmin_git.RW
  module IO = struct
    include Git_unix.Sync.IO
    let ctx () = Lwt.return_none
  end
  module Mem = struct
    module Make = Irmin_git.Mem.Make(IO)(Git_unix.Zlib)
    module KV   = Irmin_git.Mem.KV(IO)(Git_unix.Zlib)
  end
  module FS = struct
    module Make = Irmin_git.FS.Make(IO)(Git_unix.Zlib)(Git_unix.FS.IO)
    module KV   = Irmin_git.FS.KV(IO)(Git_unix.Zlib)(Git_unix.FS.IO)
  end
end

module Http = struct
  module Make = Irmin_http.Make(Cohttp_lwt_unix.Client)
  module KV (C: Irmin.Contents.S) =
    Make
      (Irmin.Metadata.None)
      (C)
      (Irmin.Path.String_list)
      (Irmin.Branch.String)
      (Irmin.Hash.SHA1)
  module Server = Irmin_http_server.Make (Cohttp_lwt_unix.Server)
end

let info ?author fmt =
  Fmt.kstrf (fun msg () ->
      let date = Int64.of_float (Unix.gettimeofday ()) in
      let author = match author with
        | Some a -> a
        | None   ->
          (* XXX: get "git config user.name" *)
          Printf.sprintf "Irmin %s.[%d]" (Unix.gethostname()) (Unix.getpid())
      in
      Irmin.Info.v ~date ~author msg
    ) fmt

let set_listen_dir_hook () =
  Irmin.Private.Watch.set_listen_dir_hook Irmin_watcher.hook
