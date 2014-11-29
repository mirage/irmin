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

module type S = sig
  include Irmin.S
  val create: path:string -> Irmin.task -> t Lwt.t
  val of_tag: path:string -> Irmin.task -> tag -> t Lwt.t
  val of_head: path:string -> Irmin.task -> head -> t Lwt.t
end

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

module Make (IO: IO)
    (K: Hash.S)
    (S: Tc.S0)
    (C: Irmin.Contents.S)
    (T: Irmin.Tag.S):
  S with type step = S.t
     and type value = C.t
     and type tag = T.t
     and type head = K.t

(** {2 Advanced configuration} *)

module type Config = sig

  (** Same as [Config] but gives more control on the file
      hierarchy. *)

  val file_of_key: string -> string
  (** Convert a key to a filename. *)

  val key_of_file: string -> string
    (** Convert a filename to a key. *)

end

module Make'(IO: IO)
    (K: Hash.S)
    (S: Tc.S0)
    (C: Irmin.Contents.S)
    (T: Irmin.Tag.S):
  S with type step = S.t
     and type value = C.t
     and type tag = T.t
     and type head = K.t
