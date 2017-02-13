(*
 * Copyright (c) 2017 Thomas Gazagnaire <thomas@gazagnaire.org>
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

open Astring
open Result

module IO = Git_mirage.Sync.IO

module type CONTEXT = sig val v: unit -> IO.ctx option Lwt.t end
module Context (C: CONTEXT) = struct
  type t = IO.ctx
  let v = C.v
end

module Irmin_git = struct
  let config = Irmin_git.config
  let head = Irmin_git.head
  let bare = Irmin_git.bare
  module AO = Irmin_git.AO
  module Memory (C: CONTEXT) = Irmin_git.Memory_ext(Context(C))(IO)
end

module Info (N: sig val name: string end) (C: Mirage_clock.PCLOCK) = struct
  let f c msg () =
    C.now_d_ps c |>
    Ptime.v |> Ptime.to_float_s |> Int64.of_float |> fun date ->
    Irmin.Info.v ~date ~owner:N.name msg
end

module KV_RO (C: CONTEXT) (I: Git.Inflate.S) = struct

  open Lwt.Infix

  module S = Irmin_git.Memory(C)(I)
      (Irmin.Contents.Cstruct)
      (Irmin.Path.String_list)
      (Irmin.Branch.String)
      (Irmin.Hash.SHA1)

  module Sync = Irmin.Sync(S)
  let config = Irmin_mem.config ()

  type 'a io = 'a Lwt.t
  type t = { path: string list; t: S.t; }
  let disconnect _ = Lwt.return_unit
  type page_aligned_buffer = Cstruct.t
  let unknown_key k = Lwt.return (Error (`Unknown_key k))
  let ok x = Lwt.return (Ok x)
  type error = Mirage_kv.error
  let pp_error = Mirage_kv.pp_error

  let read_head t =
    S.Head.find t.t >|= function
    | None   -> "empty HEAD"
    | Some h ->
      let info = S.Commit.info h in
      Fmt.strf
        "commit: %a\n\
         Author: %s\n\
         Date: %Ld\n\
         \n\
         %s\n"
        S.Commit.pp h
        (Irmin.Info.owner info)
        (Irmin.Info.date info)
        (Irmin.Info.message info)

  let mk_path t path =
    String.cuts path ~sep:"/"
    |> List.filter ((<>) "")
    |> List.append t.path

  let mem t path = S.mem t.t (mk_path t path) >|= fun res -> Ok res

  let read_store t path off len =
    S.find t.t (mk_path t path) >>= function
    | None   -> unknown_key path
    | Some v -> ok [Cstruct.sub v off len]

  let read t path off len =
    let off = Int64.to_int off
    and len = Int64.to_int len
    in
    match path with
    | "HEAD" ->
      read_head t >>= fun buf ->
      let buf = Cstruct.sub (Cstruct.of_string buf) off len in
      ok [buf]
    | _ -> read_store t path off len

  let size_head t =
    read_head t >>= fun buf -> ok (Int64.of_int @@ String.length buf)

  let size_store t path =
    S.find t.t (mk_path t path) >>= function
    | None   -> unknown_key path
    | Some v -> ok (Int64.of_int @@ Cstruct.len v)

  let size t = function
    | "HEAD" -> size_head t
    | path    -> size_store t path

  let connect ?(depth = 1) ?(branch = "master") ?path uri =
    let uri = Irmin.remote_uri (Uri.to_string uri) in
    let path = match path with
      | None -> []
      | Some s -> List.filter ((<>)"") @@ String.cuts s ~sep:"/"
    in
    S.Repo.v config >>= fun repo ->
    S.of_branch repo branch >>= fun t ->
    Sync.pull_exn t ~depth uri `Set >|= fun () ->
    { t = t; path }

end
