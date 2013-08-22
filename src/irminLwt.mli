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

(** SHA1 keys *)
module Key: KEY with type t = IrminKey.sha1

(** Blob/revision values *)
module Value: VALUE with module Key = Key

(** Basic tags *)
module Tag: TAG with type t = IrminTag.t

(** Disk access *)
module Disk: IrminDisk.S
  with module Key_store.Key = Key
   and module Value_store.Key = Key
   and module Value_store.Value = Value
   and module Tag_store.Key = Key
   and module Tag_store.Tag = Tag

(** Client bindings *)
module Client: IrminRemote.CLIENT with type t = IrminIO.Lwt_channel.t

(** Server which keeps everything into memory *)
module MemoryServer: IrminRemote.SERVER with type t = IrminIO.Lwt_channel.t

(** Server which persists everything into disk *)
module DiskServer: IrminRemote.SERVER with type t = IrminIO.Lwt_channel.t

(** Server which keeps the key-store in memory and persist tags and
    values only. *)
module MixedServer: IrminRemote.SERVER with type t = IrminIO.Lwt_channel.t
