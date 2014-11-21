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

module type ACTION = sig
  type path
  type contents
  type t =
    [ `Read of (path * contents option)
    | `Write of (path * contents option)
    | `List of (path list * path list) ]
  include Tc.I0 with type t := t
  val pretty: t -> string
end

module type S = sig
  type step
  include Ir_rw.STORE with type key = step list
  type action
  val actions: t -> action list
  val merge: t -> origin -> into:t -> unit Ir_merge.result Lwt.t
  module Action: ACTION
    with type path = key
     and type contents = value
end

module Make (S: Tc.I0) (V: Tc.I0) (O: Tc.I0): S
  with type step = S.t
   and type value = V.t
   and type origin = O.t

module type OF_STORE = sig
  include S
  type db
  val origin_of_actions: t -> origin
  val of_path: db -> origin -> key -> t Lwt.t
  val update_path: db -> origin -> key -> t -> unit Lwt.t
  val rebase_path: db -> origin -> key -> t -> unit Ir_merge.result Lwt.t
  val merge_path: db -> origin -> key -> t -> unit Ir_merge.result Lwt.t
end

module Of_store (S: Ir_bc.STORE_EXT):
  OF_STORE with type db = S.t
         and type value = S.value
         and type origin = S.origin
