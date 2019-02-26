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

(** Manage the database history. *)

module Make (K: Type.S):
  S.COMMIT with type hash = K.t

module Store
    (N: S.NODE_STORE)
    (C: sig
       include S.CONTENT_ADDRESSABLE_STORE with type key = N.key
       module Key: S.HASH with type t = key
       module Val: S.COMMIT with type t = value
                             and type hash = key
     end):
  S.COMMIT_STORE
  with type 'a t = 'a N.t * 'a C.t
   and type key = C.key
   and type value = C.value
   and type Key.t = C.Key.t
   and module Val = C.Val

module History (C: S.COMMIT_STORE):
  S.COMMIT_HISTORY with type 'a t = 'a C.t
                    and type v = C.Val.t
                    and type node = C.Node.key
                    and type commit = C.key
