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

type remote =
  | Remote: (module IrminBranch.STORE with type branch = 'a) * 'a -> remote
  | URI of string

module type STORE = sig

  (** Store with import/export capabilities. *)

  type t
  (** Local dump handler. *)

  type db
  (** Local database handlers. *)

  val create: db -> ?depth:int -> remote -> t option Lwt.t
  (** [create t last] create a dump object in the local database. The
      local database can then be either [merged] or [updated] to the
      new contents. The [depth] parameter limits the history depth.*)

  val push: db -> ?depth:int -> remote -> t option Lwt.t
  (** [push t dump] push the contents of the currnet branch of the
      database to the remote database -- also update the remote branch
      with the same name as the local one to points to the new
      state. *)

  val update: db -> t -> unit Lwt.t
  (** [update t dump] imports the contents of [dump] in the
      database. This replace the current branch with the imported
      contents.  *)

  val merge: db -> ?origin:origin -> t -> unit IrminMerge.result Lwt.t
  (** Same as [update] but merge with the current branch. *)

  val merge_exn: db -> ?origin:origin -> t -> unit Lwt.t
  (** Same as [merge] but merge raise an exception in case of conflict. *)

  val output_file: db -> ?depth:int -> string -> unit Lwt.t
  (** Create a Graphviz graph representing the store state. Could be
      no-op if the backend does not support that operation (for instance,
      for remote connections). *)

  val output_buffer: db -> ?depth:int -> Buffer.t -> unit Lwt.t
  (** Same as [output_file] but writes in a buffer. *)

  include IrminIdent.S with type t := t
  (** Base functions over database dumps. *)

end

module Make (S: IrminBranch.STORE): STORE with type db     = S.t
                                           and type t      = S.Block.key
(** Extend a branch consistent store with import/export
    capabilities. *)
