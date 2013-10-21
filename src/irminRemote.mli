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

(** Remote client/server *)

(** Signature for clients *)
module type CLIENT = sig

  type channel

  (** Clients communicate with servers using file descriptors. *)
  type t = unit -> channel Lwt.t

  (** Clients transparentely access the server store. *)
  include IrminStore.S with type t := t
                        and type Key_store.t = t
                        and type Value_store.t = t
                        and type Tag_store.t = t

  (** And they also got synchronization operations. *)
  module Sync: IrminSync.S with type t := t

end

(** Client implementation (eg. needs a server on the other side of the
    channel) *)
module Make_client (C: IrminStore.CORE): CLIENT with module Core = C

val client: string -> (module IrminStore.S)

(** Signature for servers *)
module type SERVER = sig

  (** Servers communicate with clients using file descriptors. *)
  type t

  (** How the server manage its state. *)
  module State: IrminStore.S

  (** Run the server thread *)
  val run: State.t -> ?timeout:float -> t -> unit Lwt.t

end

(** Implementation of servers. *)
module Server (S: IrminStore.S): SERVER with module State = S
