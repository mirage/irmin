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

type ('uid, 'contents) t
(** Views over keys of types ['key] and contents of type
    ['contents]. *)

type ('path, 'contents) action =
  | Read of 'path * 'contents option
  | Write of 'path * 'contents option
  | List of 'path list * 'path list
(** Operations on view. We record the result of reads to be able to
    replay them on merge. *)

module Action: sig

  (** Actions performed on a view. *)

  include Tc.I2 with type ('a, 'b) t = ('a, 'b) action

  val pretty: ('a -> string) -> ('b -> string) -> ('a, 'b) t -> string
  (** Pretty-print an action. *)

end

module type S = sig

  (** Signature for views independant of any database substrate. *)

  type value
  (** Contents value. *)

  type uid
  (** Internal unique identifiers. *)

  type path
  (** Paths to address values. *)

  include Ir_rw.S
    with type t = (uid, value) t
     and type value := value
     and type key = path

  val actions: t -> (path, value) action list
  (** Return the list of actions performed on this view since its
      creation. *)

  val merge: t -> into:t -> unit Ir_merge.result Lwt.t
  (** Merge the actions done on one view into an other one. If a read
      operation doesn't return the same result, return
      [Conflict]. Only the [into] view is updated. *)

end

module Make (U: Ir_uid.S) (C: Ir_contents.S):
  S with type value = C.t and type uid = U.t and type path = Ir_path.t
(** Create a view implementation independant of any underlying
    store. *)

module type STORE = sig

  (** Signature for views which are sub-tree of a given database
      implementation. *)

  include S

  type db
  (** Database handler. *)

  type origin
  (** Value origins. *)

  val of_path: db -> path -> t Lwt.t
  (** Read a view from a path in the store. This is a cheap operation,
      all the real reads operation will be done on-demand when the
      view is used. *)

  val update_path: ?origin:origin -> db -> path -> t -> unit Lwt.t
  (** Commit a view to the store. The view *replaces* the current
      subtree, so if you want to do a merge, you have to do it
      manually (by creating a new branch, or rebasing before
      commiting). [origin] helps keeping track of provenance. *)

  val rebase_path: ?origin:origin -> db -> path -> t -> unit Ir_merge.result Lwt.t
  (** Rebase the view to the tip of the store. *)

  val rebase_path_exn: ?origin:origin -> db -> path -> t -> unit Lwt.t
  (** Same as [rebase_path] but raise [Conflict] in case of
      conflict. *)

  val merge_path: ?origin:origin -> db -> path -> t -> unit Ir_merge.result Lwt.t
  (** Same as [update_path] but *merges* with the current subtree. *)

  val merge_path_exn: ?origin:origin -> db -> path -> t -> unit Lwt.t
  (** Same as [merge_path] but throw [Conflict "msg"] in case of
      conflict. *)

end

module Store (S: Ir_bc.S):
  STORE with type db = S.t
         and type value = S.value
         and type origin = Ir_origin.t
(** Create a view implementation tied to a the store [S]. *)
