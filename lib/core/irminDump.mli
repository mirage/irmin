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

(** Store dumps. *)

open Core_kernel.Std

type origin = IrminOrigin.t

type ('key, 'contents) t = {
  head : 'key option;
  store: ('key * ('key, 'contents) IrminBlock.t) list;
}
(** Dump values. *)

module type S = sig

  (** Signature for dump values .*)

  type key
  (** Keys. *)

  type contents
  (** Contents. *)

  include IrminIdent.S with type t = (key, contents) t
  (** Base functions over dump values. *)

end

module S (K: IrminKey.S) (C: IrminContents.S): S with type key = K.t and type contents = C.t
(** Base functions over dump values. *)

module type STORE = sig

  (** Store with import/export capabilities. *)

  type key
  (** Database internal keys. *)

  type contents
  (** User-defined contents. *)

  type dump = (key, contents) t
  (** Database dumps. *)

  type t
  (** Database handlers. *)

  val create: t -> key list -> dump Lwt.t
  (** [create t last] returns the new contents stored in [t] since the
      [last] snaphots has been taken. If no previous snapshots
      are provided, return the full contents of the store. *)

  val update: t -> dump -> unit Lwt.t
  (** [update t dump] imports the contents of [dump] in the
      database. This replace the current branch with the imported
      contents.  *)

  val merge: t -> ?origin:origin -> dump -> unit IrminMerge.result Lwt.t
  (** Same as [update] but merge with the current branch. *)

  val merge_exn: t -> ?origin:origin -> dump -> unit Lwt.t
  (** Same as [merge] but merge raise an exception in case of conflict. *)

  val output: t -> string -> unit Lwt.t
  (** Create a Graphviz graph representing the store state. Could be
      no-op if the backend does not support that operation (for instance,
      for remote connections). *)

  module Dump: S with type key = key and type contents = contents
  (** Base functions over database dumps. *)

end

module Make (S: IrminBranch.STORE):
  STORE with type t        = S.t
         and type key      = S.Block.key
         and type contents = S.Block.contents
(** Extend a branch consistent store with import/export
    capabilities. *)
