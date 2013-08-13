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

(** Base irminsule datatypes *)

(** {2 Basic types} *)

(** Keys *)
type key = K of string

(** Blobs *)
type blob = B of string

(** Revisions *)
type revision = {
  parents : key list;
  contents: key;
}

(** Values *)
type value =
  | Blob of blob
  | Revision of revision

(** Local tags *)
type local_tag = L of string

(** Remote tags *)
type remote_tag = R of string

(** Tags *)
type tag =
  [ `Local of local_tag
  | `Remote of remote_tag ]

(** Keys *)
module Key (C: IrminAPI.CHANNEL): IrminAPI.KEY
  with type t = key
   and type channel = C.t

(** Values *)
module Value (C: IrminAPI.CHANNEL): IrminAPI.VALUE
  with type t = value
   and type channel = C.t

(** Tags *)
module Tag (C: IrminAPI.CHANNEL):  IrminAPI.TAG
  with type t = tag
   and type channel = C.t

(** {2 Helpers} *)

(** List of elements *)
module MakeList
    (C: IrminAPI.CHANNEL)
    (E: IrminAPI.BASE with type channel = C.t)
  : IrminAPI.BASE
    with type t = E.t list
     and type channel = C.t

(** Cartesian product *)
module MakeProduct
    (C: IrminAPI.CHANNEL)
    (K: IrminAPI.BASE with type channel = C.t)
    (V: IrminAPI.BASE with type channel = C.t)
  : IrminAPI.BASE with type t = K.t * V.t
                   and type channel = C.t
