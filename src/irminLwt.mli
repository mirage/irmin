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

module Key: KEY with type t = IrminKey.sha1

module Value: VALUE with type key = Key.t

module Tag: TAG with type t = IrminTag.t

module Key_store: KEY_STORE with type key = Key.t

module Value_store: VALUE_STORE with type key = Key.t and type value = Value.t

module Tag_store: TAG_STORE with type key = Key.t and type tag = Tag.t

module Client: (module type of IrminProtocol.Client(Key)(Value)(Tag))

module MemoryServer: (module type of
                       IrminProtocol.Server
                         (Key)(Value)(Tag)
                         (Key_store)(Value_store)(Tag_store)
                     )

module Disk: (module type of IrminProtocol.Disk(Key)(Value)(Tag))
