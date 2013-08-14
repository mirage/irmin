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

(** Queue Implementation *)

open IrminTypes

(** Signature *)
module type S = sig

  (** Abstract type for channels *)
  type channel

  (** Implementation of keys *)
  module Key: KEY

  (** Implementation of values *)
  module Value: VALUE

  (** Implementation of tags *)
  module Tag: TAG

  (** Implementation of clients *)
  module Client: REMOTE with type key = Key.t
                         and type tag = Tag.t
                         and type channel = channel

  (** Implementation of servers (just need the dispatch function) *)
  module Server
      (KS: KEY_STORE with type key = Key.t)
      (TS: TAG_STORE with type key = Key.t and type tag = Tag.t):
  sig
    val dispatch: channel -> unit Lwt.t
  end

end

module Make (C: CHANNEL) (K: IrminKey.S with type channel = C.t):
  S with type channel = C.t
     and type Key.t = K.t
