(*
 * Copyright (c) 2013-2014 Thomas Gazagnaire <thomas@gazagnaire.org>
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

module type STORE = sig
  type step
  include Ir_bc.STORE with type key = step list
  module Key: Ir_path.S with type step = step
  module Val: Ir_contents.S with type t = value
  module Tag: Ir_tag.S with type t = tag
  module Head: Ir_hash.S with type t = head
  module Private: sig
    include Ir_bc.PRIVATE
      with type Contents.value = value
       and type Node.Path.step = step
       and type Commit.key = head
       and type Tag.key = tag
       and type Slice.t = slice
    val contents_t: t -> Contents.t
    val node_t: t -> Contents.t * Node.t
    val commit_t: t -> Contents.t * Node.t * Commit.t
    val tag_t: t -> Tag.t
    val read_node: t -> key -> Node.value option Lwt.t
    val mem_node: t -> key -> bool Lwt.t
    val update_node: t -> key -> Node.value -> unit Lwt.t
  end
end

module type MAKER =
  functor (P: Ir_path.S) ->
  functor (C: Ir_contents.S) ->
  functor (T: Ir_tag.S) ->
  functor (H: Ir_hash.S) ->
    STORE with type step = P.step
           and type value = C.t
           and type tag = T.t
           and type head = H.t

module Make_ext (P: Ir_bc.PRIVATE) = struct
  module P = Ir_bc.Make_ext(P)
  include (P: module type of P with module Private := P.Private
                                and module Tag := P.Tag)
  module Tag = P.Tag.Key
  module Head = P.Commit.Key
  module Private = struct
    include P.Private
    let contents_t = P.contents_t
    let node_t = P.node_t
    let commit_t = P.commit_t
    let tag_t = P.tag_t
    let update_node = P.update_node
    let mem_node = P.mem_node
    let read_node = P.read_node
  end
end

module Make
    (AO: Ir_ao.MAKER)
    (RW: Ir_rw.MAKER)
    (P: Ir_path.S)
    (C: Ir_contents.S)
    (T: Ir_tag.S)
    (H: Ir_hash.S) =
struct
  module X = struct
    module Contents = struct
      module Key = H
      module Val = C
      include AO (Key)(Val)
    end
    module Node = struct
      module Key = H
      module Val = Ir_node.Make (H)(H)(P)
      module Path = P
      include AO (Key)(Val)
    end
    module Commit = struct
      module Key = H
      module Val = Ir_commit.Make (H)(H)
      include AO (Key)(Val)
    end
    module Tag = struct
      module Key = T
      module Val = H
      include RW (Key)(Val)
    end
    module Slice = Ir_slice.Make(Contents)(Node)(Commit)(Tag)
  end
  include Make_ext(X)
end
