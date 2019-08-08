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

module type S = sig
  include Irmin_git.S with type Private.Sync.endpoint = Git_unix.endpoint

  val remote : ?headers:Cohttp.Header.t -> string -> Irmin.remote
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

module Make : S_MAKER

module KV : KV_MAKER

module Ref : REF_MAKER

module FS : sig
  module G : Irmin_git.G

  module Make (C : Irmin.Contents.S) (P : Irmin.Path.S) (B : Irmin.Branch.S) :
    S
      with type key = P.t
       and type step = P.step
       and module Key = P
       and type contents = C.t
       and type branch = B.t
       and module Git = G

  module Ref (C : Irmin.Contents.S) :
    S
      with type key = string list
       and type step = string
       and type contents = C.t
       and type branch = Irmin_git.reference
       and module Git = G

  module KV (C : Irmin.Contents.S) :
    S
      with type key = Irmin.Path.String_list.t
       and type step = string
       and module Key = Irmin.Path.String_list
       and type contents = C.t
       and type branch = string
       and module Git = G
end

module Mem : sig
  module G : Irmin_git.G

  module Make (C : Irmin.Contents.S) (P : Irmin.Path.S) (B : Irmin.Branch.S) :
    S
      with type key = P.t
       and type step = P.step
       and module Key = P
       and type contents = C.t
       and type branch = B.t
       and module Git = G

  module Ref (C : Irmin.Contents.S) :
    S
      with type key = string list
       and type step = string
       and type contents = C.t
       and type branch = Irmin_git.reference
       and module Git = G

  module KV (C : Irmin.Contents.S) :
    S
      with type key = Irmin.Path.String_list.t
       and type step = string
       and module Key = Irmin.Path.String_list
       and type contents = C.t
       and type branch = string
       and module Git = G
end
