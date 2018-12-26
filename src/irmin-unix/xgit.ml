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
module Log =(val Logs.src_log src : Logs.LOG)

module type S = sig
  include Irmin_git.S with type Private.Sync.endpoint = Git_unix.endpoint
  val remote: ?headers:Cohttp.Header.t -> string -> Irmin.remote
end

module type S_MAKER = functor
  (G: Irmin_git.G)
  (C: Irmin.Contents.S)
  (P: Irmin.Path.S)
  (B: Irmin.Branch.S) ->
  S with type key = P.t
     and type step = P.step
     and module Key = P
     and type contents = C.t
     and type branch = B.t
     and module Git = G

module type KV_MAKER = functor
  (G: Irmin_git.G)
  (C: Irmin.Contents.S) ->
  S with type key = string list
     and type step = string
     and type contents = C.t
     and type branch = string
     and module Git = G

module type REF_MAKER = functor
  (G: Irmin_git.G)
  (C: Irmin.Contents.S) ->
  S with type key = string list
     and type step = string
     and type contents = C.t
     and type branch = Irmin_git.reference
     and module Git = G

module Make
    (G: Irmin_git.G)
    (C: Irmin.Contents.S)
    (P: Irmin.Path.S)
    (B: Irmin.Branch.S)
= struct
  include Irmin_git.Make(G)(Git_unix.Sync(G))(C)(P)(B)
  let remote ?headers uri =
    let e = Git_unix.endpoint ?headers (Uri.of_string uri) in
    E e
end

module KV (G: Irmin_git.G) (C: Irmin.Contents.S) = struct
  include Irmin_git.KV(G)(Git_unix.Sync(G))(C)
  let remote ?headers uri =
    let e = Git_unix.endpoint ?headers (Uri.of_string uri) in
    E e
end

module Ref (G: Irmin_git.G) (C: Irmin.Contents.S) = struct
  include Irmin_git.Ref(G)(Git_unix.Sync(G))(C)
  let remote ?headers uri =
    let e = Git_unix.endpoint ?headers (Uri.of_string uri) in
    E e
end

module FS = struct

  module G = struct
    include Git_unix.Store
    let v ?dotgit ?compression ?buffers root =
      let buffer = match buffers with
        | None   -> None
        | Some p -> Some (Lwt_pool.use p)
      in
      v ?dotgit ?compression ?buffer root
  end

  module Make = Make(G)
  module Ref  = Ref(G)
  module KV   = KV(G)
end

module Mem = struct
  module G    = Irmin_git.Mem
  module Content_addressable = Irmin_git.Content_addressable(G)
  module Atomic_write = Irmin_git.Atomic_write(G)
  module Make = Make(G)
  module Ref  = Ref(G)
  module KV   = KV(G)
end
