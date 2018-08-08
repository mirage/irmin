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

module Net = Git_mirage.Net

module NoConduit = struct
  include Conduit_mirage
  let context = Conduit_mirage.empty
  let resolver = Resolver_lwt.init ()
end

module X = struct
  module AO = Irmin_git.AO
  module Make (C: Net.CONDUIT) = Irmin_git.Make(Net.Make(C))
  module Ref  (C: Net.CONDUIT) = Irmin_git.Ref(Net.Make(C))
  module KV   (C: Net.CONDUIT) = Irmin_git.KV(Net.Make(C))
end

module Info (N: sig val name: string end) (C: Mirage_clock.PCLOCK) = struct
  let f c fmt =
    Fmt.kstrf (fun msg () ->
        C.now_d_ps c |>
        Ptime.v |> Ptime.to_float_s |> Int64.of_float |> fun date ->
        Irmin.Info.v ~date ~author:N.name msg
      ) fmt
end

module KV_RO (G: Git.S) = struct

  module G = struct
    include G
    let v ?dotgit:_ ?compression:_ ?buffers:_ _root =
      assert false
  end

  open Lwt.Infix

  module S = X.KV(NoConduit)(G)(Irmin.Contents.Cstruct)

  module Sync = Irmin.Sync(S)

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
        (Irmin.Info.author info)
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

  let connect ?(depth = 1) ?(branch = "master") ?path t uri =
    let uri = Irmin.remote_uri (Uri.to_string uri) in
    let path = match path with
      | None -> []
      | Some s -> List.filter ((<>)"") @@ String.cuts s ~sep:"/"
    in
    let head = G.Reference.of_string ("refs/heads/" ^ branch) in
    S.repo_of_git ~bare:true ~head t >>= fun repo ->
    S.of_branch repo branch >>= fun t ->
    Sync.pull_exn t ~depth uri `Set >|= fun () ->
    { t = t; path }

end

module Git = X
