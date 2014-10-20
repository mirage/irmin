(*
 * Copyright (c) 2014 Thomas Gazagnaire <thomas@gazagnaire.org>
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

(** Watches *)

module type S = sig

  (** Collection of listeners. *)

  type key
  (** Keys. *)

  type value
  (** Values. *)

  type t
  (** Listeners state. *)

  val notify: t -> key -> value option -> unit
  (** Notifity all listeners that a key havs been changed. If the
      argument is [None], this means the key has been removed. *)

  val create: unit -> t
  (** Create in-memory notifications. *)

  val clear: t -> unit
  (** Clear all watches. *)

  val watch: t -> key -> value option -> value Lwt_stream.t
  (** Create a stream of event notifications. Need to provide the
      initial value (or [None] if the key does not have associated
      contents yet).  *)

  val listen_dir: t -> string
    -> key:(string -> key option)
    -> value:(key -> value option Lwt.t)
    -> unit
  (** Register a fsevents/inotify thread to look for changes in the
      given directory. *)

end

module Make(K: Key.S) (V: sig type t end):
  S with type key = K.t
     and type value = V.t

val set_listen_dir_hook: (string -> (string -> unit Lwt.t) -> unit) -> unit
(** Register a function which looks for file changes in a
    directory. *)
