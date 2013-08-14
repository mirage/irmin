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

(** Blobs *)

open IrminTypes

(** Signature for value implementations *)
module type S = sig
  include VALUE
  include IO with type t := t
end

(** Default blob type *)
type blob = B of string

(** Default blob implementation *)
module Blob (C: CHANNEL): S with type t = blob
                             and type channel = C.t

(** Default revision type *)
type 'a revision = {
  parents : 'a list;
  contents: 'a;
}

module Revision (C: CHANNEL) (K: IrminKey.S with type channel = C.t):
  S with type t = K.t revision
     and type channel = C.t

(** Default value type *)
type 'a t =
  | Blob of blob
  | Revision of 'a revision

(** Default value implementation *)
module Make (C: CHANNEL) (K: IrminKey.S with type channel = C.t):
  S with type t = K.t t
     and type channel = C.t
