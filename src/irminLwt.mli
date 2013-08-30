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

(** Implementation of the Irminsule protocol using Lwt channels *)

open IrminTypes

(** {2 Core} *)

(** Types of keys. *)
module Key: KEY with type t = IrminKey.SHA1.t and type Set.t = IrminKey.SHA1.Set.t

(** Types of values. *)
module Value: VALUE with module Key = Key

(** Types of tags. *)
module Tag: TAG with type t = IrminTag.t

(** Core types. *)
module C: CORE with module Key = Key
                and module Value = Value
                and module Tag = Tag

(** {2 Store} *)

 (** Disk access. *)
module Disk: IrminDisk.S with module C = C

(** Memory acces. *)
module Memory: IrminMemory.S with module C = C

(** Client bindings *)
module Client: IrminRemote.CLIENT with type t = IrminIO.Lwt_channel.t

(** Generic store handle. *)
type handle =
  | Disk of Disk.t
  | Memory of Memory.t
  | Client of Client.t

(** Generic store. *)
type t = {
  keys  : handle;
  values: handle;
  tags  : handle;
}

(** Generic store implementation. *)
include STORE with type t := t
               and module C := C

(** Store source. *)
type source =
  | Dir of string
  | Unix of string
  | InMemory

(** Store creation. *)
val create: keys:source -> values:source -> tags:source -> t Lwt.t

(** {2 Servers} *)

(** Server which keeps everything into memory *)
module MemoryServer: IrminRemote.SERVER with type t = IrminIO.Lwt_channel.t
                                         and module State := Memory

(** Server which persists everything into disk *)
module DiskServer: IrminRemote.SERVER with type t = IrminIO.Lwt_channel.t
                                       and module State := Disk

(** Server which keeps the key-store in memory and persist tags and
    values only. *)
module MixServer: IrminRemote.SERVER with type t = IrminIO.Lwt_channel.t
