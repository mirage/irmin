(*
 * Copyright (c) 2013-2015 Thomas Gazagnaire <thomas@gazagnaire.org>
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
  include Ir_bc.STORE
  module Key: Ir_path.S with type t = key
  module Val: Ir_contents.S with type t = value
  module Ref: Ir_tag.S with type t = branch_id
  module Head: Ir_hash.S with type t = head
  module Private: sig
    include Ir_bc.PRIVATE
      with type Contents.value = value
       and type Commit.key = head
       and type Ref.key = branch_id
       and type Slice.t = slice
       and module Contents.Path = Key
    val repo: t -> Repo.t
    val contents_t: t -> Contents.t
    val node_t: t -> Node.t
    val commit_t: t -> Commit.t
    val ref_t: t -> Ref.t
    val read_node: t -> key -> Node.key option Lwt.t
    val mem_node: t -> key -> bool Lwt.t
    val update_node: t -> key -> Node.key -> unit Lwt.t
    val merge_node: t -> key -> (head * Node.key) -> unit Ir_merge.result Lwt.t
    val remove_node: t -> key -> unit Lwt.t
    val iter_node: t -> Node.key ->
      (key -> value Lwt.t -> unit Lwt.t) -> unit Lwt.t
  end
end

module type MAKER =
  functor (C: Ir_contents.S) ->
  functor (R: Ir_tag.S) ->
  functor (H: Ir_hash.S) ->
    STORE with type key = C.Path.t
           and type value = C.t
           and type branch_id = R.t
           and type head = H.t

module Make (AO: Ir_ao.MAKER) (RW: Ir_rw.MAKER): MAKER

module Make_ext (P: Ir_bc.PRIVATE): STORE
  with type key = P.Contents.Path.t
   and type value = P.Contents.value
   and type branch_id = P.Ref.key
   and type head = P.Ref.value
   and type Key.step = P.Contents.Path.step
