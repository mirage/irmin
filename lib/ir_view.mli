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
  (** Operations on view. We record the result of reads to be able to
      replay them on merge. *)

  include Tc.I0 with type t := t

  val pretty: t -> string
  (** Pretty-print an action. *)
end

module type S = sig

  (** Signature for views independant of any database substrate. *)

  type step

  include Ir_rw.STORE with type key = step list

  type action
  (** The type for actions. *)

  val actions: t -> action list
  (** Return the list of actions performed on this view since its
      creation. *)

  val merge: t -> origin -> into:t -> unit Ir_merge.result Lwt.t
  (** Merge the actions done on one view into an other one. If a read
      operation doesn't return the same result, return
      [Conflict]. Only the [into] view is updated. *)

  module Action: ACTION
    with type path = key
     and type contents = value
  (** Base functions over actions. *)

end

module Make (S: Tc.I0) (V: Tc.I0) (O: Tc.I0): S
  with type step = S.t
   and type value = V.t
   and type origin = O.t
(** Create a view implementation independant of any underlying
    store. *)

module type OF_STORE = sig

  (** Signature for views which are sub-tree of a given database
      implementation. *)

  include S

  type db
  (** Database handler. *)

  val origin_of_actions: t -> origin
  (** Create an origin using the list of actions as message. *)

  val of_path: db -> origin -> key -> t Lwt.t
  (** Read a view from a path in the store. This is a cheap operation,
      all the real reads operation will be done on-demand when the
      view is used. *)

  val update_path: db -> origin -> key -> t -> unit Lwt.t
  (** Commit a view to the store. The view *replaces* the current
      subtree, so if you want to do a merge, you have to do it
      manually (by creating a new branch, or rebasing before
      commiting). [origin] helps keeping track of provenance. *)

  val rebase_path: db -> origin -> key -> t -> unit Ir_merge.result Lwt.t
  (** Rebase the view to the tip of the store. *)

  val merge_path: db -> origin -> key -> t -> unit Ir_merge.result Lwt.t
  (** Same as [update_path] but *merges* with the current subtree. *)

end

module Of_store (S: Ir_bc.STORE_EXT):
  OF_STORE with type db = S.t
         and type value = S.value
         and type origin = S.origin
(** Create a view implementation tied to a the store [S]. *)
