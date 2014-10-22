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

(** Disk persistence. *)

(** {2 Simple configuration} *)

module type Config = sig

  (** Simple configuration containing the store location on the
      filesystem. *)

  val path: string

end

(** {2 Advanced configuration} *)

module type Config' = sig

  (** Same as [Config] but gives more control on the file
      hierarchy. *)

  val path: string
  (** The database root. *)

  val file_of_key: string -> string
  (** Convert a key to a filename. *)

  val key_of_file: string -> string
    (** Convert a filename to a key. *)

end

(** {2 Constructors} *)

module type IO = sig

  (** File-system abstraction. *)

  val check_dir: string -> unit Lwt.t
  (** Check than a given dirname exists and is indeed a directory
      name. *)

  val with_file_in: string -> (Cstruct.t -> 'a Lwt.t) -> 'a Lwt.t
  (** Run a function on the contents of a file. *)

  val rec_files: string -> string list
  (** Get all the files in a sub-tree. *)

  val with_file_out: string -> Cstruct.t -> unit Lwt.t
  (** Write a new file with the given contents. *)

  val remove_file: string -> unit Lwt.t
  (** Remove a file. *)

end

module Make (IO: IO) (C: Config) : Irmin.Sig.BACKEND
module Make'(IO: IO) (C: Config'): Irmin.Sig.BACKEND
