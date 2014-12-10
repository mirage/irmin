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

(** Irmin stores *)

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
    val node_t: t -> Node.t
    val commit_t: t -> Commit.t
    val tag_t: t -> Tag.t
    val read_node: t -> key -> Node.key option Lwt.t
    val mem_node: t -> key -> bool Lwt.t
    val update_node: t -> key -> Node.key -> unit Lwt.t
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

module Make (AO: Ir_ao.MAKER) (RW: Ir_rw.MAKER): MAKER

module Make_ext (P: Ir_bc.PRIVATE): STORE
  with type step = P.Node.Path.step
   and type value = P.Contents.value
   and type tag = P.Tag.key
   and type head = P.Tag.value
