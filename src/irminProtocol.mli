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

(** Client implementation (eg. needs a server on the other side of the
    channel) *)
module Client (K: KEY) (V: VALUE with type key = K.t) (T: TAG): sig
  module Key_store: KEY_STORE with type t = Lwt_unix.file_descr
  module Value_store: VALUE_STORE with type t = Lwt_unix.file_descr
  module Tag_store: TAG_STORE with type t = Lwt_unix.file_descr
  module Sync: SYNC with type t = Lwt_unix.file_descr
end

(** Signature for server *)
module type SERVER = sig

  (** The channel to reach the server *)
  type channel = Lwt_unix.file_descr

  (** The channel the server is using to reach the key store *)
  type key_store

  (** The channel the server is using to reach the tag store *)
  type tag_store

  (** The channel the server is using to reach the value store *)
  type value_store

  (** Server type  *)
  type t = {
    keys  : key_store;
    tags  : tag_store;
    values: value_store;
  }

  (** Run the server thread *)
  val run: t -> channel -> unit Lwt.t

end

(** Implementation of servers. Expose a process function. *)
module Server
    (K: KEY) (V: VALUE with type key = K.t) (T: TAG)
    (KS: KEY_STORE with type key = K.t)
    (VS: VALUE_STORE with type key = K.t and type value = V.t)
    (TS: TAG_STORE with type key = K.t and type tag = T.t)
  : SERVER with type key_store = KS.t
            and type value_store = VS.t
            and type tag_store = TS.t

(** Disk *)
module Disk (K: KEY) (V: VALUE with type key = K.t) (T: TAG): sig
  type t
  val with_file: string -> (t -> 'a) -> 'a
  val init: string -> unit Lwt.t
  module Key_store: KEY_STORE with type t = t
  module Value_store: VALUE_STORE with type t = t
  module Tag_store: TAG_STORE with type t = t
end
