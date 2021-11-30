(*
 * Copyright (c) 2021 Craig Ferguson <craig@tarides.com>
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
open Store_properties

open struct
  module type Node_portable = Node.Portable.S
  module type Commit_portable = Commit.Portable.S
end

module type S = sig
  module Schema : Schema.S

  module Hash : Hash.S with type t = Schema.Hash.t
  (** Internal hashes. *)

  (** Backend content store. *)

  module Contents :
    Contents.Store with type hash = Hash.t and type value = Schema.Contents.t

  (** Backend node store. *)

  module Node :
    Node.Store
      with type hash = Hash.t
       and type Val.contents_key = Contents.key
       and module Path = Schema.Path
       and module Metadata = Schema.Metadata

  module Node_portable :
    Node_portable
      with type node := Node.value
       and type hash := Hash.t
       and type metadata := Schema.Metadata.t
       and type step := Schema.Path.step

  (** Backend commit store. *)

  module Commit :
    Commit.Store
      with type hash = Hash.t
       and type Val.node_key = Node.key
       and module Info = Schema.Info

  module Commit_portable :
    Commit_portable
      with type commit := Commit.value
       and type hash := Hash.t
       and module Info = Schema.Info

  (** Backend branch store. *)
  module Branch :
    Branch.Store with type key = Schema.Branch.t and type value = Commit.key

  (** Backend slices. *)
  module Slice :
    Slice.S
      with type contents = Contents.hash * Contents.value
       and type node = Node.hash * Node.value
       and type commit = Commit.hash * Commit.value

  (** Backend repositories. *)
  module Repo : sig
    type t

    include Of_config with type _ t := t
    (** @inline *)

    include Closeable with type _ t := t
    (** @inline *)

    val contents_t : t -> read Contents.t
    val node_t : t -> read Node.t
    val commit_t : t -> read Commit.t
    val branch_t : t -> Branch.t

    val batch :
      t ->
      (read_write Contents.t ->
      read_write Node.t ->
      read_write Commit.t ->
      'a Lwt.t) ->
      'a Lwt.t
  end

  (** URI-based low-level remote synchronisation. *)
  module Remote : sig
    include Remote.S with type commit = Commit.key and type branch = Branch.key

    val v : Repo.t -> t Lwt.t
  end
end
