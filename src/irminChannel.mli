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

(** Channels *)

module type S = sig

  (** Signature for IO channels. *)

  type t
  (** Type of IO channels. *)

  val name: t -> string
  (** Channel name. *)

  val read_string: t -> int -> string Lwt.t
  (** Read a string on a channel. *)

  val write_string: t -> string -> unit Lwt.t
  (** Write a string on a channel. *)

  val read_ba: t -> int -> Cstruct.buffer Lwt.t
  (** [read_ba chan len] reads a bigarray of size [len] from the
      channel [chan]. *)

  val write_ba: t -> Cstruct.buffer -> unit Lwt.t
  (** [write_ba chan ba] writes the bigarray [ba] on the channel
      [chan]. *)

  val read_length: t -> int Lwt.t
  (** Read the contents length. *)

  val write_length: t -> int -> unit Lwt.t
  (** Read the contents length. *)

  val read_unit: t -> unit Lwt.t
  (** Read an unit value on a channel. *)

  val write_unit: t -> unit Lwt.t
  (** Write an unit value on a channel. *)

  val close: t -> unit Lwt.t
  (** Close a channel. *)

  (** {2 File system abstraction (XXX: move it)} *)

  val of_file: string -> t Lwt.t
  (** Open a file. *)

  val file_exits: string -> bool Lwt.t
  (** Does a file exists. *)

  val is_directory: string -> bool Lw.t
  (** Is the file a directory. *)

  val mkdir: string -> unit Lwt.t
  (** Create a directory. *)

end

module Make (B: IrminBase.S) (C: S): IrminBase.SC
  with type t = B.t
   and type channel = C.t
(** Extend a base implementation with serializability functions. *)

module Lwt_unix: sig

  include S

  (** Create a channel from a file-descriptor and a name (useful for
      debug purposes only(. *)
  val create: Lwt_unix.file_descr -> string -> t

end
