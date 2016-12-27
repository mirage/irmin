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

module Make (C: Tc.S0) (N: Tc.S0):
  Ir_s.COMMIT with type commit = C.t and type node = N.t

module Store
    (N: Ir_s.NODE_STORE)
    (S: sig
       include Ir_s.AO_STORE
       module Key: Ir_s.HASH with type t = key
       module Val: Ir_s.COMMIT with type t = value
                                and type commit = key
                                and type node = N.key
     end):
  Ir_s.COMMIT_STORE
    with  type t = N.t * S.t
      and type key = S.key
      and type value = S.value
      and module Key = S.Key
      and module Val = S.Val

module type HISTORY = sig
  type t
  type node
  type commit
  val create: t -> node:node -> parents:commit list -> task:Ir_task.t -> commit Lwt.t
  val node: t -> commit -> node Lwt.t
  val parents: t -> commit -> commit list Lwt.t
  val merge: t -> task:Ir_task.t -> commit Ir_merge.t
  val lcas: t -> ?max_depth:int -> ?n:int -> commit -> commit ->
    [`Ok of commit list | `Max_depth_reached | `Too_many_lcas ] Lwt.t
  val lca: t -> task:Ir_task.t -> ?max_depth:int -> ?n:int -> commit list -> commit option Ir_merge.result Lwt.t
  val three_way_merge: t -> task:Ir_task.t -> ?max_depth:int -> ?n:int -> commit -> commit -> commit Ir_merge.result Lwt.t
  val closure: t -> min:commit list -> max:commit list -> commit list Lwt.t
end

module History (S: Ir_s.COMMIT_STORE):
  HISTORY with type t = S.t
           and type node = S.Node.key
           and type commit = S.key
