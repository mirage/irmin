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

(** Signature for clients *)
module type CLIENT = sig

  (** Abstract channels *)
  type t

  (** Access the remote key store *)
  module Key_store: KEY_STORE with type t = t

  (** Access the remote value store *)
  module Value_store: VALUE_STORE with type t = t

  (** Access the remote tag store *)
  module Tag_store: TAG_STORE with type t = t

  (** Sync with a remote server *)
  module Sync: SYNC with type t = t

end


(** Client implementation (eg. needs a server on the other side of the
    channel) *)
module Client (K: KEY) (V: VALUE with module Key = K) (T: TAG)
  : CLIENT with type t = Lwt_unix.file_descr

(** Signature for servers *)
module type SERVER = sig

  (** Abstract channels between the client and server *)
  type t

  (** The server handles to manage its key store *)
  module Key_store: KEY_STORE

  (** The server handles to manage its value store *)
  module Value_store: VALUE_STORE

  (** The server handles to manage its tag store *)
  module Tag_store: TAG_STORE

  (** All the server handlers  *)
  type stores = {
    keys  : Key_store.t;
    values: Value_store.t;
    tags  : Tag_store.t;
  }

  (** Run the server thread *)
  val run: stores -> t -> unit Lwt.t

end

(** Implementation of servers. Expose a process function. *)
module Server
    (K: KEY) (V: VALUE with module Key = K) (T: TAG)
    (KS: KEY_STORE with module Key = K)
    (VS: VALUE_STORE with module Key = K and module Value = V)
    (TS: TAG_STORE with module Key = K and module Tag = T)
  : SERVER with module Key_store = KS
            and module Value_store = VS
            and module Tag_store = TS
            and type t = Lwt_unix.file_descr
