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

let src = Logs.Src.create "git.unix" ~doc:"logs git's unix events"

module Log = (val Logs.src_log src : Logs.LOG)

module type S = sig
  include
    Irmin_git.S
      with type Private.Sync.endpoint = Mimic.ctx * Smart_git.Endpoint.t

  val remote :
    ?ctx:Mimic.ctx -> ?headers:Cohttp.Header.t -> string -> Irmin.remote
end

module type S_MAKER = functor
  (G : Irmin_git.G)
  (C : Irmin.Contents.S)
  (P : Irmin.Path.S)
  (B : Irmin.Branch.S)
  ->
  S
    with type key = P.t
     and type step = P.step
     and module Key = P
     and type contents = C.t
     and type branch = B.t
     and module Git = G

module type KV_MAKER = functor (G : Irmin_git.G) (C : Irmin.Contents.S) ->
  S
    with type key = string list
     and type step = string
     and type contents = C.t
     and type branch = string
     and module Git = G

module type REF_MAKER = functor (G : Irmin_git.G) (C : Irmin.Contents.S) ->
  S
    with type key = string list
     and type step = string
     and type contents = C.t
     and type branch = Irmin_git.reference
     and module Git = G

let remote ?(ctx = Mimic.empty) ?headers uri =
  let ( ! ) f a b = f b a in
  let headers = Option.map Cohttp.Header.to_list headers in
  match Smart_git.Endpoint.of_string uri with
  | Ok edn ->
      let edn =
        Option.fold ~none:edn
          ~some:(!Smart_git.Endpoint.with_headers_if_http edn)
          headers
      in
      (ctx, edn)
  | Error (`Msg err) -> Fmt.invalid_arg "remote: %s" err

module Make
    (G : Irmin_git.G)
    (C : Irmin.Contents.S)
    (P : Irmin.Path.S)
    (B : Irmin.Branch.S) =
struct
  include Irmin_git.Make (G) (Git_unix.Sync (G)) (C) (P) (B)

  let remote ?ctx ?headers uri = E (remote ?ctx ?headers uri)
end

module KV (G : Irmin_git.G) (C : Irmin.Contents.S) = struct
  include Irmin_git.KV (G) (Git_unix.Sync (G)) (C)

  let remote ?ctx ?headers uri = E (remote ?ctx ?headers uri)
end

module Ref (G : Irmin_git.G) (C : Irmin.Contents.S) = struct
  include Irmin_git.Ref (G) (Git_unix.Sync (G)) (C)

  let remote ?ctx ?headers uri = E (remote ?ctx ?headers uri)
end

module FS = struct
  module G = Git_unix.Store
  module S = Git_unix.Sync (G)
  module Make = Make (G)
  module Ref = Ref (G)
  module KV = KV (G)
end

module Mem = struct
  module G = Irmin_git.Mem
  module S = Git.Mem.Sync (G)
  module Content_addressable = Irmin_git.Content_addressable (G)
  module Atomic_write = Irmin_git.Atomic_write (G)
  module Make = Make (G)
  module Ref = Ref (G)
  module KV = KV (G)
end
