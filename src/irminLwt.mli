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

module Key: IrminKey.S
  with type channel = Lwt_unix.file_descr

module Value: IrminValue.S
  with type channel = Lwt_unix.file_descr
   and type t = Key.t IrminValue.t

module Tag: IrminTag.S
  with type channel = Lwt_unix.file_descr

module Client: OPERATIONS
  with type key = IrminKey.t
   and type t = Lwt_unix.file_descr

module MemoryServer: IrminProtocol.SERVER
   with type channel = Lwt_unix.file_descr
    and module KS = IrminMemory.Key_store(Key)
    and module VS = IrminMemory.Value_store(Key)(Value)
    and module TS = IrminMemory.Tag_store(Tag)(Key)

module Disk: OPERATIONS
  with type key = IrminKey.t
   and type t = Lwt_unix.file_descr
