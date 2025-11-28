(*
 * Copyright (c) 2013-2022 Thomas Gazagnaire <thomas@gazagnaire.org>
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

module Conf : sig
  open Irmin.Backend.Conf

  val spec : Spec.t

  module Key : sig
    val root : string key
  end
end

val config : string -> Irmin.config
(** [config root] is the a configuration with the key {!Irmin.Config.root} set
    to [root]. **)

module type IO = sig
  (** {1 File-system abstractions} *)

  type io

  val io_of_config : Irmin.config -> io

  type path = string
  (** The type for paths. *)

  (** {2 Read operations} *)

  val rec_files : io:io -> path -> path list
  (** [rec_files dir] is the list of files recursively present in [dir] and all
      of its sub-directories. Return filenames prefixed by [dir]. *)

  val file_exists : io:io -> path -> bool
  (** [file_exist f] is true if [f] exists. *)

  val read_file : io:io -> path -> string option
  (** Read the contents of a file using mmap. *)

  (** {2 Write Operations} *)

  val mkdir : io:io -> path -> unit
  (** Create a directory. *)

  type lock
  (** The type for file locks. *)

  val lock_file : io:io -> path -> lock
  (** [lock_file f] is the lock associated to the file [f]. *)

  val write_file :
    io:io -> temp_dir:path -> ?lock:lock -> path -> string -> unit
  (** Atomic writes. *)

  val test_and_set_file :
    io:io ->
    temp_dir:path ->
    lock:lock ->
    path ->
    test:string option ->
    set:string option ->
    bool
  (** Test and set. *)

  val remove_file : io:io -> ?lock:lock -> path -> unit
  (** Remove a file or directory (even if non-empty). *)
end

module Append_only (IO : IO) : Irmin.Append_only.Maker
module Atomic_write (IO : IO) : Irmin.Atomic_write.Maker
module Maker (IO : IO) : Irmin.Maker
module KV (IO : IO) : Irmin.KV_maker with type info = Irmin.Info.default

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

  val clear : unit -> unit
  val set_listen_hook : unit -> unit
end
