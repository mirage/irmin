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

(** Protocol implementation *)

open IrminTypes

module Disk (C: CHANNEL) (K: IrminKey.S with type channel = C.t):
  OPERATIONS with type key = K.t
              and type t = C.t

(** Client implementation (eg. needs a server on the other side of the
    channel) *)
module Client (C: CHANNEL) (K: IrminKey.S with type channel = C.t):
  OPERATIONS with type key = K.t
              and type t = C.t

(** Signature for server *)
module type SERVER = sig

  (** The channel to reach the server *)
  type channel

  (** The channel the server is using to reach the key store *)
  module KS: KEY_STORE

  (** The channel the server is using to reach the tag store *)
  module TS: TAG_STORE

  (** The channel the server is using to reach the value store *)
  module VS: VALUE_STORE

  (** Server type  *)
  type t = {
    keys  : KS.t;
    tags  : TS.t;
    values: VS.t;
  }

  (** Run the server thread *)
  val run: t -> channel -> unit Lwt.t

end

(** Implementation of servers. Expose a process function. *)
module Server
    (C: CHANNEL)
    (K: IrminKey.S with type channel = C.t)
    (KS: KEY_STORE with type key = K.t)
    (TS: TAG_STORE with type key = K.t and type tag = IrminTag.t)
    (VS: VALUE_STORE with type key = K.t and type value = K.t IrminValue.t):
  SERVER with type channel = C.t
          and module KS = KS
          and module TS = TS
          and module VS = VS
