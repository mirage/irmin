(*
 * Copyright (c) 2013-2015 Thomas Gazagnaire <thomas@gazagnaire.org>
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

val config: ?config:Irmin.config -> ?root:string -> unit -> Irmin.config
(** [config ?config ~root ()] is the configuration [config] augmented
    with the key {!Irmin.Config.root} set to [root]. If not specified,
    [config] is {!Irmin.Config.empty}. *)

module type IO = sig

  (** File-system abstraction. *)

  val getcwd: unit -> string Lwt.t
  (** Return the current directory. *)

  val mkdir: string -> unit Lwt.t
  (** Create a directory. *)

  val remove: string -> unit Lwt.t
  (** Remove a file or directory (even if non-empty). *)

  val rec_files: string -> string list Lwt.t
  (** [rec_files dir] is the list of files recursively present in
      [dir] and all of its sub-directories. Return filenames prefixed
      by [dir].  *)

  val file_exists: string -> bool Lwt.t
  (** [file_exist f] is true if [f] exists. *)

  val read_file: string -> Cstruct.t Lwt.t
  (** Read the contents of a file using mmap. *)

  val write_file: string -> ?temp_dir:string -> Cstruct.t -> unit Lwt.t
  (** Write some contents to a new file. *)

end

module type LOCK = sig

  (** {1 Filesystem {i dotlocking}} *)

  val with_lock: string -> (unit -> 'a Lwt.t) -> 'a Lwt.t
  (** [with_lock file fn] runs [fn] while holding a lock on the file
      [file]. *)

end

module AO (IO: IO): Irmin.AO_MAKER
module Link (IO: IO): Irmin.LINK_MAKER
module RW (IO: IO) (L: LOCK): Irmin.RW_MAKER
module Make (IO: IO) (L: LOCK): Irmin.S_MAKER

(** {2 Advanced configuration} *)

module type Config = sig

  (** Same as [Config] but gives more control on the file
      hierarchy. *)

  val dir: string -> string
  (** [dir root] is the sub-directory to look for the keys. *)

  val file_of_key: string -> string
  (** Convert a key to a filename. *)

  val key_of_file: string -> string
    (** Convert a filename to a key. *)

end

module AO_ext (IO: IO) (C: Config): Irmin.AO_MAKER
module Link_ext (IO: IO) (C: Config): Irmin.LINK_MAKER
module RW_ext (IO: IO) (L: LOCK) (C: Config): Irmin.RW_MAKER
module Make_ext (IO: IO) (L: LOCK) (Obj: Config) (Ref: Config): Irmin.S_MAKER
