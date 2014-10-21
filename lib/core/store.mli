(*
 * Copyright (c) 2013-2014 Thomas Gazagnaire <thomas@gazagnaire.org>
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

(** API entry point *)

module type S = sig

  (** Main signature for Irmin stores. *)

  include Branch.STORE with type key = Path.t

  module Dump: Dump.S with type t = t
  module Snapshot: Snapshot.STORE with type db = t
  module View: View.STORE with type db    = t
                           and type node  = Block.key
                           and type value = value
  module Sync: Sync.STORE with type db  = t
end

type ('key, 'contents, 'tag) t =
  (module S with type Block.key = 'key
             and type value     = 'contents
             and type branch    = 'tag)

val cast: ('a, 'b, 'c) t -> (module S)

module Make
    (Block   : Block.STORE)
    (Tag     : Tag.STORE with type value = Block.key)
  : S with type Block.key = Block.key
       and type value     = Block.contents
       and type branch    = Tag.key
(** Build a full iminsule store. *)

(** {2 Backends} *)

module type RO_MAKER =
  functor (K: Key.S)   ->
  functor (V: Tc.I0) ->
    Sig.RO with type key = K.t and type value = V.t

module type AO_MAKER =
  functor (K: Key.S)   ->
  functor (V: Tc.I0) ->
    Sig.AO with type key = K.t and type value = V.t

module type RW_MAKER =
  functor (K: Key.S) ->
  functor (V: Key.S) ->
    Sig.RW with type key = K.t and type value = V.t

module type S_MAKER =
  functor (K: Key.S)      ->
  functor (C: Contents.S) ->
  functor (T: Tag.S)      ->
    S with type Block.key = K.t
       and type value     = C.t
       and type branch    = T.t

(** {2 Backends} *)
module type BACKEND = sig
  module RO  : RO_MAKER
  module AO  : AO_MAKER
  module RW  : RW_MAKER
  module Make: S_MAKER
end

(** {2 Recursive stores} *)

module Rec (AO: AO_MAKER) (S: S): S with type value = S.Block.key
(** Recursive store, where contents are pointers to arbitrary blocks
    living in the store. *)

(** {2 Binary stores} *)

module RO_BINARY (S: Sig.RO_BINARY): RO_MAKER
module AO_BINARY (S: Sig.AO_BINARY): AO_MAKER
module RW_BINARY (S: Sig.RW_BINARY): RW_MAKER

module Binary
    (AO: Sig.AO_BINARY)
    (RW: Sig.RW_BINARY)
    (K: Key.S) (C: Contents.S) (T: Tag.S)
  : S with type Block.key = K.t
       and type value     = C.t
       and type branch    = T.t
(** Create an irminsule store from binary stores. Use one common
    append-only store for contents, nodes and commits and a mutable
    store for the tags. *)
