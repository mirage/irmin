(*
 * Copyright (c) 2013-2021 Thomas Gazagnaire <thomas@gazagnaire.org>
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

val config : string -> Irmin.config
(** [config root] is the a configuration with the key {!Irmin.Config.root} set
    to [root]. **)

module type IO = sig
  (** {1 File-system abstractions} *)

  type path = string
  (** The type for paths. *)

  (** {2 Read operations} *)

  val rec_files : path -> string list Lwt.t
  (** [rec_files dir] is the list of files recursively present in [dir] and all
      of its sub-directories. Return filenames prefixed by [dir]. *)

  val file_exists : path -> bool Lwt.t
  (** [file_exist f] is true if [f] exists. *)

  val read_file : path -> string option Lwt.t
  (** Read the contents of a file using mmap. *)

  (** {2 Write Operations} *)

  val mkdir : path -> unit Lwt.t
  (** Create a directory. *)

  type lock
  (** The type for file locks. *)

  val lock_file : path -> lock
  (** [lock_file f] is the lock associated to the file [f]. *)

  val write_file : ?temp_dir:path -> ?lock:lock -> path -> string -> unit Lwt.t
  (** Atomic writes. *)

  val test_and_set_file :
    ?temp_dir:string ->
    lock:lock ->
    path ->
    test:string option ->
    set:string option ->
    bool Lwt.t
  (** Test and set. *)

  val remove_file : ?lock:lock -> path -> unit Lwt.t
  (** Remove a file or directory (even if non-empty). *)
end

module Append_only (IO : IO) : Irmin.Append_only.Maker
module Atomic_write (IO : IO) : Irmin.Atomic_write.Maker
module Maker (IO : IO) : Irmin.Maker
module KV (IO : IO) : Irmin.KV_maker

(** {2 Advanced configuration} *)

module type Config = sig
  (** Same as [Config] but gives more control on the file hierarchy. *)

  val dir : string -> string
  (** [dir root] is the sub-directory to look for the keys. *)

  val file_of_key : string -> string
  (** Convert a key to a filename. *)

  val key_of_file : string -> string
  (** Convert a filename to a key. *)
end

module Append_only_ext (IO : IO) (C : Config) : Irmin.Append_only.Maker
module Atomic_write_ext (IO : IO) (C : Config) : Irmin.Atomic_write.Maker
module Maker_ext (IO : IO) (Obj : Config) (Ref : Config) : Irmin.Maker

(** {1 In-memory IO mocks} *)

module IO_mem : sig
  include IO

  val clear : unit -> unit Lwt.t
  val set_listen_hook : unit -> unit
end
