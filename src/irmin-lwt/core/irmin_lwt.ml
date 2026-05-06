(*
 * Copyright (c) 2013-2022 Thomas Gazagnaire <thomas@gazagnaire.org>
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

open! Import
module Type = Repr
module Metrics = Metrics
module Diff = Diff
module Read_only = Read_only
module Append_only = Append_only
module Indexable = Indexable
module Content_addressable = Content_addressable
module Atomic_write = Atomic_write
module Contents = Contents
module Merge = Merge
module Branch = Branch
module Node = Node
module Commit = Commit
module Info = Info
module Schema = Schema
module Dot = Dot.Make
module Hash = Hash
module Path = Path
module Perms = Perms
module Key = Key
module Irmin_node = Node

exception Closed = Store_properties.Closed

(* [Generic_key.Maker] (a functor producing a [Generic_key.S] from four
   per-store sub-Makers) is intentionally not exposed: the only way to route
   it through Irmin 4 is to bridge each user-supplied Lwt sub-Maker to its
   Eio counterpart, which forces every operation through a Lwt -> Eio -> Lwt
   round-trip. The functor was unused in [irmin-lwt]'s own surface (backends
   like [irmin-lwt-pack] implement the [Generic_key.Maker] signature
   directly via [Wrap_store.Make]). The module type [Generic_key.Maker]
   stays exposed because backends use it as their public signature; only
   the functor implementation is gone. See LIMITATIONS.md. *)

module Maker (CA : Content_addressable.Maker) (AW : Atomic_write.Maker) = struct
  type endpoint = unit
  type ('h, _) contents_key = 'h
  type 'h node_key = 'h
  type 'h commit_key = 'h

  module Make (S : Schema.S) = Maker_v2.Make (CA) (AW) (S)
end

module KV_maker (CA : Content_addressable.Maker) (AW : Atomic_write.Maker) =
struct
  type metadata = unit
  type hash = Schema.default_hash
  type info = Info.default

  module Maker = Maker (CA) (AW)
  include Maker
  module Make (C : Contents.S) = Maker.Make (Schema.KV (C))
end

(* [Of_backend] is intentionally not exposed: routing a Lwt-typed [Backend.S]
   through Irmin 4 (Eio) would require ~400 lines of sub-store bridges with
   per-op overhead, and no application user of [irmin-lwt] needs the entry
   point. See [irmin_lwt.mli] for the rationale and the recommended
   workaround for users who do need a custom backend. *)

module type Tree = Tree.S
module type S = Store.S

type config = Conf.t
type 'a diff = 'a Diff.t

module type Maker = Store.Maker
module type KV = Store.KV
module type KV_maker = Store.KV_maker

module Generic_key = struct
  include Store.Generic_key
end

module Backend = struct
  module Conf = Conf
  module Slice = Slice
  module Remote = Remote

  module type S = Backend.S

  module Watch = Watch
  module Lock = Lock
  module Lru = Lru
end

let version = Irmin.version

module Sync = Sync

type remote = Remote.t = ..

let remote_store (type t) (module M : Generic_key.S with type t = t) (t : t) =
  let module X : Store.Generic_key.S with type t = t = M in
  Sync.remote_store (module X) t

module Metadata = Metadata
module Json_tree = Store.Json_tree
module Export_for_backends = Export_for_backends
module Storage = Storage

module Of_storage (M : Storage.Make) (H : Hash.S) (V : Contents.S) = struct
  module CA = Storage.Content_addressable (M)
  module AW = Storage.Atomic_write (M)
  module Maker = Maker (CA) (AW)

  include Maker.Make (struct
    module Hash = H
    module Contents = V
    module Info = Info.Default
    module Metadata = Metadata.None
    module Path = Path.String_list
    module Branch = Branch.String
    module Node = Node.Make (Hash) (Path) (Metadata)
    module Commit = Commit.Make (Hash)
  end)
end

(* Shim helpers exposed for downstream backend packages. *)
module Lwt_to_eio = Lwt_to_eio
module Wrap_store = Wrap_store
