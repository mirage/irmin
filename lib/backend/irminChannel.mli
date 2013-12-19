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
  (** Type of named channels. *)

  type channel
  (** Type of concrete channels. *)

  val create: channel -> string -> t
  (** Create a named channel. *)

  val name: t -> string
  (** Channel name. *)

  val channel: t -> channel
  (** Return the channel. *)

  val read_string: t -> int -> string Lwt.t
  (** Read a string on a channel. *)

  val write_string: t -> string -> unit Lwt.t
  (** Write a string on a channel. *)

  val read_bigarray: t -> int -> Cstruct.buffer Lwt.t
  (** [read_bigarray chan len] reads a bigarray of size [len] from the
      channel [chan]. *)

  val write_bigarray: t -> Cstruct.buffer -> unit Lwt.t
  (** [write_ba chan ba] writes the bigarray [ba] on the channel
      [chan]. *)

  val read_buffer: t -> Mstruct.t Lwt.t
  (** [read_buffer chan] reads the buffer encoded on the channel. *)

  val write_buffer: t -> Mstruct.t -> unit Lwt.t
  (** [write_buffer chan buf] writes the buffer [buf] to the channel
      [chan]. *)

  val read_contents_length: t -> int Lwt.t
  (** Read the contents length. XXX: 32 bits only! *)

  val write_contents_length: t -> int -> unit Lwt.t
  (** Read the contents length. XXX: 32 bits only! *)

  val read_unit: t -> unit Lwt.t
  (** Read an unit value on a channel. *)

  val write_unit: t -> unit Lwt.t
  (** Write an unit value on a channel. *)

  val close: t -> unit Lwt.t
  (** Close a channel. *)

end

include S with type channel = Lwt_unix.file_descr

(** Create a channel from a file-descriptor and a name (useful for
    debug purposes only(. *)
val create: Lwt_unix.file_descr -> string -> t
