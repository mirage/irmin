(*
 * Copyright (c) 2018-2021 Tarides <contact@tarides.com>
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

module Make (Conf : Conf.S) (Sc : Irmin.Schema.S) :
  Schema.Node
    with type hash = Sc.hash
     and type step = Sc.step
     and type metadata = Sc.metadata

module Store
    (Sc : Schema.S)
    (C : Irmin.Contents.Store
           with type key = Sc.hash
            and type value = Sc.contents)
    (N : Content_addressable.S
           with type key = Sc.hash
            and type value = Sc.Node.Raw.t) : sig
  type key = Sc.hash
  type value = Sc.node

  module Val : sig
    include
      Irmin.Node.S
        with type t = value
         and type metadata = Sc.metadata
         and type hash = Sc.hash
         and type step = Sc.step

    (* FIXME: only needed for the tests. *)
    val values :
      t -> [ `Node of hash | `Inode of hash | `Contents of hash ] list
  end

  include
    Irmin.Node.Store
      with type key := key
       and type value := value
       and type 'a t = 'a C.t * 'a N.t
       and module Path = Sc.Path
       and module Metadata = Sc.Metadata
       and module Contents = C
       and module Val := Val

  (** @inline *)
  include
    S.Checkable
      with type 'a t := 'a t
       and type key := key
       and type value := value
end

module Schema (Conf : Conf.S) (Sc : Irmin.Schema.S) :
  Schema.S
    with type hash = Sc.hash
     and type branch = Sc.branch
     and type info = Sc.info
     and type commit = Sc.commit
     and type metadata = Sc.metadata
     and type step = Sc.step
     and type path = Sc.path
     and type contents = Sc.contents
