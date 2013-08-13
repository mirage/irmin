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

(** Concrete keys *)
module Key: IrminAPI.KEY
  with type channel = Lwt_unix.file_descr
   and type t = IrminImpl.key

(** Concrete values *)
module Value: IrminAPI.VALUE
  with type channel = Lwt_unix.file_descr
   and type t = IrminImpl.value

(** Concrete tags *)
module Tag: IrminAPI.TAG
  with type channel = Lwt_unix.file_descr
   and type t = IrminImpl.tag

(** Concrete low-level store *)
module Store: IrminAPI.STORE
  with module K = Key
   and module V = Value

(** Concrete tag store *)
module Tag_store: IrminAPI.TAG_STORE
  with module T = Tag
   and module K = Key
