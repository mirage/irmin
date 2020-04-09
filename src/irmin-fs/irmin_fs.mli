(*
 * Copyright (c) 2013-2017 Thomas Gazagnaire <thomas@gazagnaire.org>
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

(** [config ?config root] is the configuration [config] augmented with the key
    {!Irmin.Config.root} set to [root]. If not specified, [config] is
    {!Irmin.Config.empty}. *)
val config : ?config:Irmin.config -> string -> Irmin.config

module type IO = sig
  (** {1 File-system abstractions} *)

  (** The type for paths. *)
  type path = string

  (** {2 Read operations} *)

  (** [rec_files dir] is the list of files recursively present in [dir] and all
      of its sub-directories. Return filenames prefixed by [dir]. *)
  val rec_files : path -> string list Lwt.t

  (** [file_exist f] is true if [f] exists. *)
  val file_exists : path -> bool Lwt.t

  (** Read the contents of a file using mmap. *)
  val read_file : path -> string option Lwt.t

  (** {2 Write Operations} *)

  (** Create a directory. *)
  val mkdir : path -> unit Lwt.t

  (** The type for file locks. *)
  type lock

  (** [lock_file f] is the lock associated to the file [f]. *)
  val lock_file : path -> lock

  (** Atomic writes. *)
  val write_file : ?temp_dir:path -> ?lock:lock -> path -> string -> unit Lwt.t

  (** Test and set. *)
  val test_and_set_file :
    ?temp_dir:string ->
    lock:lock ->
    path ->
    test:string option ->
    set:string option ->
    bool Lwt.t

  (** Remove a file or directory (even if non-empty). *)
  val remove_file : ?lock:lock -> path -> unit Lwt.t
end

module Append_only (IO : IO) : Irmin.APPEND_ONLY_STORE_MAKER

module Atomic_write (IO : IO) : Irmin.ATOMIC_WRITE_STORE_MAKER

module Make (IO : IO) : Irmin.S_MAKER

module KV (IO : IO) : Irmin.KV_MAKER

(** {2 Advanced configuration} *)

module type Config = sig
  (** Same as [Config] but gives more control on the file hierarchy. *)

  (** [dir root] is the sub-directory to look for the keys. *)
  val dir : string -> string

  (** Convert a key to a filename. *)
  val file_of_key : string -> string

  (** Convert a filename to a key. *)
  val key_of_file : string -> string
end

module Append_only_ext (IO : IO) (C : Config) : Irmin.APPEND_ONLY_STORE_MAKER

module Atomic_write_ext (IO : IO) (C : Config) : Irmin.ATOMIC_WRITE_STORE_MAKER

module Make_ext (IO : IO) (Obj : Config) (Ref : Config) : Irmin.S_MAKER

(** {1 In-memory IO mocks} *)

module IO_mem : sig
  include IO

  val clear : unit -> unit Lwt.t

  val set_listen_hook : unit -> unit
end
