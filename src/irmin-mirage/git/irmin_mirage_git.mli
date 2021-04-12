(*
 * Copyright (c) 2013-2021 Thomas Gazagnaire <thomas@gazagnaire.org>
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

include Irmin_mirage_git_intf.Sigs
module Maker (G : Irmin_git.G) : Maker with module G := G

module KV (G : Irmin_git.G) :
  KV_maker with type branch = string and module G := G

module Ref (G : Irmin_git.G) :
  KV_maker with type branch = Irmin_git.reference and module G := G

(** Functor to create a MirageOS' KV_RO store from a Git repository. The key
    ["/HEAD"] always shows the current HEAD. *)
module KV_RO (G : Irmin_git.G) : KV_RO with type git := G.t

(** Functor to create a MirageOS' KV_RW store from a Git repository. *)
module KV_RW (G : Irmin_git.G) (C : Mirage_clock.PCLOCK) :
  KV_RW with type git := G.t

(** Embed an Irmin store into an in-memory Git repository. *)
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

  module KV_RO : KV_RO with type git := G.t
  module KV_RW (C : Mirage_clock.PCLOCK) : KV_RW with type git := G.t
end
