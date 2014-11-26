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

(** In-memory partial views of the database, with lazy fetching. *)

module type S = sig
  type step
  include Ir_rw.STORE with type key = step list
  val create: Ir_task.t -> t
  val merge: t -> into:t -> unit Ir_merge.result Lwt.t
  type db
  val of_path: db -> key -> t Lwt.t
  val update_path: db -> key -> t -> unit Lwt.t
  val rebase_path: db -> key -> t -> unit Ir_merge.result Lwt.t
  val merge_path: db -> key -> t -> unit Ir_merge.result Lwt.t
  module Action: sig
    type t =
      [ `Read of (key * value option)
      | `Write of (key * value option)
      | `List of (key list * key list) ]
    include Tc.S0 with type t := t
    val pretty: t -> string
    val prettys: t list -> string
  end
  val actions: t -> Action.t list
end

module Make (S: Ir_bc.STORE_EXT):
  S with type db = S.t
     and type step = S.step
     and type value = S.value
