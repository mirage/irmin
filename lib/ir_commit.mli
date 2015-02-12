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

(** Manage the database history. *)

module type S = sig
  include Tc.S0
  type commit
  type node
  val create: Ir_task.t -> ?node:node -> parents:commit list -> t
  val node: t -> node option
  val parents: t -> commit list
  val task: t -> Ir_task.t
end

module Make (C: Tc.S0) (N: Tc.S0):
  S with type commit := C.t
     and type node = N.t

module type STORE = sig
  include Ir_ao.STORE
  module Key: Ir_hash.S with type t = key
  module Val: S
    with type t = value
     and type commit := key
end

module type HISTORY = sig
  type t
  type node
  type commit
  val create: t -> ?node:node -> parents:commit list -> commit Lwt.t
  val node: t -> commit -> node option Lwt.t
  val parents: t -> commit -> commit list Lwt.t
  val merge: t -> commit Ir_merge.t
  val lcas: t -> ?max_depth:int -> ?n:int -> commit -> commit ->
    [`Ok of commit list | `Max_depth_reached | `Too_many_lcas ] Lwt.t
  val lca: t -> ?max_depth:int -> ?n:int -> commit list -> commit option Ir_merge.result Lwt.t
  val three_way_merge: t -> ?max_depth:int -> ?n:int -> commit -> commit -> commit Ir_merge.result Lwt.t
  val closure: t -> min:commit list -> max:commit list -> commit list Lwt.t
  module Store: Ir_contents.STORE with type t = t and type key = commit
end

module History (N: Ir_contents.STORE) (S: STORE with type Val.node = N.key):
  HISTORY with type t = N.t * S.t
           and type node = N.key
           and type commit = S.key
