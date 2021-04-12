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

(** Git backend *)

module Metadata = Metadata
module Conf = Conf
module Branch = Branch
module Reference = Reference

(** {2 Modules types} *)

include Irmin_git_intf.Sigs
(** @inline *)

val config :
  ?config:Irmin.config ->
  ?head:Git.Reference.t ->
  ?bare:bool ->
  ?level:int ->
  ?dot_git:string ->
  ?buffers:int ->
  string ->
  Irmin.config

type reference = Reference.t [@@deriving irmin]

module Content_addressable (G : Git.S) : sig
  (** Use Git as a content-addressable store. Values will be stored into
      [.git/objects].*)

  module type S = Irmin.Content_addressable.S with type key = G.Hash.t

  module Make (V : Irmin.Type.S) : S with type value = V.t
end

module Atomic_write (G : Git.S) : sig
  (** Use Git as an atomic-write store. Values will be stored into [.git/refs].
      When using the Git filesystem backend, branch names .*)

  module type S = Irmin.Atomic_write.S with type value = G.Hash.t

  module Make (K : Irmin.Branch.S) : S with type key = K.t
end

module Maker
    (G : G)
    (S : Git.Sync.S with type hash := G.hash and type store := G.t) :
  Maker with module G := G and type endpoint = Mimic.ctx * Smart_git.Endpoint.t

module KV
    (G : G)
    (S : Git.Sync.S with type hash := G.hash and type store := G.t) :
  KV_maker
    with module G := G
     and type branch = string
     and type endpoint = Mimic.ctx * Smart_git.Endpoint.t

module Ref
    (G : G)
    (S : Git.Sync.S with type hash := G.hash and type store := G.t) :
  KV_maker
    with module G := G
     and type branch = Reference.t
     and type endpoint = Mimic.ctx * Smart_git.Endpoint.t

(** Same as {!Maker} but with a custom branch implementation. *)
module Maker_ext
    (G : G)
    (S : Git.Sync.S with type hash := G.hash and type store := G.t) : sig
  module Make (C : Irmin.Contents.S) (P : Irmin.Path.S) (B : Branch.S) :
    S
      with type key = P.t
       and type step = P.step
       and module Key = P
       and type contents = C.t
       and type branch = B.t
       and type Private.Remote.endpoint = Mimic.ctx * Smart_git.Endpoint.t
       and module Git = G
end

module Generic
    (CA : Irmin.Content_addressable.Maker)
    (AW : Irmin.Atomic_write.Maker) : sig
  type endpoint

  module Make (C : Irmin.Contents.S) (P : Irmin.Path.S) (B : Irmin.Branch.S) :
    Irmin.S
      with type key = P.t
       and type step = P.step
       and module Key = P
       and type contents = C.t
       and type branch = B.t
       and type Private.Remote.endpoint = endpoint
end

module Generic_KV
    (CA : Irmin.Content_addressable.Maker)
    (AW : Irmin.Atomic_write.Maker) : Irmin.KV_maker with type endpoint = unit

(** In-memory Git store. *)
module Mem :
  G with type t = Digestif.SHA1.t Git.Mem.t and type hash = Digestif.SHA1.t
