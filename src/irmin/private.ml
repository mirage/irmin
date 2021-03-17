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
open S.Store_properties

module type S = sig
  type +'a io
  type 'a merge

  module IO : IO.S with type 'a t = 'a io
  module Merge : Merge.S with type 'a io = 'a io and type 'a t = 'a merge

  type 'a io := 'a IO.t
  type 'a merge := 'a Merge.t

  module Hash : Hash.S
  (** Internal hashes. *)

  (** Private content store. *)
  module Contents :
    Contents.STORE
      with type key = Hash.t
       and type 'a io := 'a io
       and type 'a merge := 'a merge

  (** Private node store. *)
  module Node :
    Node.STORE
      with type key = Hash.t
       and type 'a io := 'a io
       and type 'a merge := 'a merge

  (** Private commit store. *)
  module Commit :
    Commit.STORE
      with type key = Hash.t
       and type 'a io := 'a io
       and type 'a merge := 'a merge

  (** Private branch store. *)
  module Branch : Branch.STORE with type value = Hash.t and type 'a io := 'a io

  (** Private slices. *)
  module Slice :
    Slice.S
      with type 'a io := 'a IO.t
       and type contents = Contents.key * Contents.value
       and type node = Node.key * Node.value
       and type commit = Commit.key * Commit.value

  (** Private repositories. *)
  module Repo : sig
    type t

    (** @inline *)
    include OF_CONFIG with type _ t := t and type 'a io := 'a io

    (** @inline *)
    include CLOSEABLE with type _ t := t and type 'a io := 'a io

    val contents_t : t -> read Contents.t
    val node_t : t -> read Node.t
    val commit_t : t -> read Commit.t
    val branch_t : t -> Branch.t

    val batch :
      t ->
      (read_write Contents.t ->
      read_write Node.t ->
      read_write Commit.t ->
      'a io) ->
      'a io
  end

  (** URI-based low-level sync. *)
  module Sync : sig
    include
      Sync.S
        with type commit = Commit.key
         and type branch = Branch.key
        with type 'a io := 'a io

    val v : Repo.t -> t io
  end
end
