(*
 * Copyright (c) 2013 Thomas Gazagnaire <thomas@gazagnaire.org>
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

(** Base irminsule datatypes *)

(** Basic types *)
module Types: sig

  (** Keys *)
  type key = K of string

  (** Blobs *)
  type blob = B of string

  (** Revisions *)
  type revision = {
    parents : key list;
    contents: key;
  }

  (** Values *)
  type value =
    | Blob of blob
    | Revision of revision

  (** Local tags *)
  type local_tag = L of string

  (** Remote tags *)
  type remote_tag = R of string

end

open Types

module Channel: IrminAPI.CHANNEL
  with type t = Lwt_unix.file_descr

(** Keys *)
module Key: IrminAPI.KEY
  with type t = key
   and type channel = Channel.t

(** Values *)
module Value: IrminAPI.VALUE
  with type t = value
   and type channel = Channel.t

(** Blobs *)
module Blob: IrminAPI.BASE
  with type t = blob
   and type channel = Channel.t

(** Revisions *)
module Revision: IrminAPI.BASE
  with type t = revision
   and type channel = Channel.t

(** {2 Helpers} *)

(** Arguements for the [Iter] functor *)
module type IterArg = sig

  (** Abstract type of constucted values *)
  type t

  (** How to read a value *)
  val read: Channel.t -> t Lwt.t

  (** How to write a value *)
  val write: Channel.t -> t -> unit Lwt.t
end

(** Build (binary) iterators from single readers/writers *)
module Iter(A: IterArg): sig

  (** Read a list of values *)
  val reads: Channel.t -> A.t list Lwt.t

  (** Write a list of values *)
  val writes: Channel.t -> A.t list -> unit Lwt.t

end
