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

(** Manage the database history. *)

module type S = sig
  include Tc.S0
  type commit
  type node
  val create: Ir_task.t -> ?node:node -> parents:commit list -> t
  val node: t -> node option
  val parents: t -> commit list
  val task: t -> Ir_task.t
  val edges: t -> [> `Node of node | `Commit of commit] list
end

module Make (C: Tc.S0) (N: Tc.S0):
  S with type commit := C.t
     and type node = N.t

module type STORE = sig

  include Ir_ao.STORE

  module Key: Ir_hash.S with type t = key
  (** Base functions over keys. *)

  module Val: S
    with type t = value
     and type commit := key
  (** Base functions over values. *)

end

module type STORE_EXT = sig
  module Node: Ir_node.STORE_EXT
  include STORE with type Val.node = Node.key
  type node = Node.value
  val commit: t -> ?node:node -> parents:value list -> (key * value) Lwt.t
  val node: t -> value -> node Lwt.t option
  val parents: t -> value -> value Lwt.t list
  val merge: t -> key Ir_merge.t
  val find_common_ancestor: t -> key -> key -> key option Lwt.t
  val find_common_ancestor_exn: t -> key -> key -> key Lwt.t
  val node_t: t -> Node.t
  val rec_list: t -> key list -> key list Lwt.t
end

module Make_ext
    (C: Ir_contents.STORE)
    (N: Ir_node.STORE with type Val.contents = C.key)
    (S: STORE with type Val.node = N.key):
  STORE_EXT with type t = C.t * N.t * S.t
             and type key = S.key
             and type value = S.value
             and module Node = Ir_node.Make_ext(C)(N)
