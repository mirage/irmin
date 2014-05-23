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

open Core_kernel.Std

type ('key, 'contents) t
(** Views over keys of types ['key] and contents of type
    ['contents]. *)

module Action: sig

  (** Actions performed on a view. *)
  type 'contents t =
    | Read of IrminPath.t * 'contents option
    | Write of IrminPath.t * 'contents option
    | List of IrminPath.t list * IrminPath.t list
  with bin_io, compare, sexp
  (** Operations on view. We record the result of reads to be able to
      replay them on merge. *)

  val to_string: ('a -> string) -> 'a t -> string
  (** Pretty-print an action. *)

end

module type S = sig

  type value
  (** Contents value. *)

  type node
  (** Internal nodes. *)

  include IrminStore.RW
    with type t = (node, value) t
     and type value := value
     and type key = IrminPath.t

  val import:
    contents:(node -> value option Lwt.t) ->
    node:(node -> node IrminNode.t option Lwt.t) ->
    node -> t Lwt.t
  (** Create a rooted view from a database node. *)

  val export:
    contents:(value -> node Lwt.t) ->
    node:(node IrminNode.t -> node Lwt.t) ->
    t -> node Lwt.t
  (** Export the view to the database. *)

  val actions: t -> value Action.t list
  (** Return the list of actions performed on this view since its
      creation. *)

  val merge: t -> into:t -> unit IrminMerge.result Lwt.t
  (** Merge the actions done on one view into an other one. If a read
      operation doesn't return the same result, return
      [Conflict]. Only the [into] view is updated. *)

end

module Make (K: IrminKey.S) (C: IrminContents.S)
  : S with type value = C.t
       and type node = K.t
